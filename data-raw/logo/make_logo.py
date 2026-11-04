#!/usr/bin/env python3
"""Erzeugt das PQDM-Hexlogo als SVG.

Motiv: Oszilloskop-Schirm mit Grundschwingung und 3. Harmonischer.
Zwei Perioden der Grundschwingung (weiss, dunkler Leuchtrand) und die
3. Harmonische mit einem Drittel der Amplitude (dunkel, weisser Leuchtrand).

Die 3. Harmonische ist um 60 Grad phasenverschoben. Bei 0 Grad fallen die
Nulldurchgaenge beider Kurven zusammen, bei 90 Grad liegen die Maxima der
Harmonischen auf den Nulldurchgaengen der Grundschwingung; beides wirkt zu
symmetrisch.

    python make_logo.py > logo.svg
    python make_logo.py 240 orange > orange.svg   # andere Farbe der Familie

PNG-Export (transparenter Hintergrund):

    rsvg-convert -w 240 -o logo.png logo.svg
"""

import math
import sys

# ---------------------------------------------------------------- Geometrie
W = 200.0                       # Hexagon-Breite; Hoehe = W * 2/sqrt(3)
FONT = "Fira Sans, Source Sans Pro, Helvetica Neue, Arial, sans-serif"

# Farbfamilie: gleiche Helligkeit und Saettigung (OKLCH), nur der Farbton
# wechselt. PQDM ist petrol, die uebrigen Farben sind fuer Erweiterungen und
# Schwesterpakete reserviert. Stufen je Farbton:
#   bg     L 0.62  Grund, Weiss darauf hat Kontrast ~3.5-3.8 (fette Schrift)
#   border L 0.54  Rand, eine Stufe dunkler als der Grund
#   line   L 0.44  Raster und 3. Harmonische, dunkler als der Grund
FAMILY = dict(
    petrol=dict(bg="#0099ae", border="#007f92", line="#005f6e"),  # PQDM
    berry=dict(bg="#bd658a", border="#9f5172", line="#783c55"),
    brick=dict(bg="#c4685a", border="#a55347", line="#7d3d34"),
    orange=dict(bg="#bf6e3e", border="#a1592e", line="#794220"),
    purple=dict(bg="#9772c0", border="#7e5da2", line="#5e457a"),
)


def palette(name="petrol"):
    f = FAMILY[name]
    return dict(bg=f["bg"], grid=f["line"], border=f["border"],
                sine=f["line"], lead="#ffffff", title="#ffffff")


PAL = palette("petrol")

# Motivparameter
PERIODS = 2                                # Perioden der Grundschwingung
H3, PH3 = 1 / 3, 60.0                      # 3. Harmonische: rel. Amplitude, Phase (Grad)
FUND_W, H3_W = 4.2, 2.6                    # Strichstaerken
FUND_GLOW = ("#003a44", 5.0, 0.60)         # Leuchtrand Grundschwingung: Farbe, Breite, Deckkraft
H3_GLOW = ("#ffffff", 4.0, 0.55)           # Leuchtrand 3. Harmonische
BLUR = 1.6                                 # Weichzeichnung der Leuchtraender
AMP, Y0 = 36.0, -16.0                      # Amplitude, Nulllinie
X0, X1 = -104.0, 104.0                     # Spur laeuft ueber die Kante
BORDER_W = 8.0
TITLE, TITLE_Y, TITLE_SIZE, TITLE_LS = "PQDM", 64.0, 36.0, 4.0


def hex_path(cx, cy, w):
    h = w * 2 / math.sqrt(3)
    pts = [(cx, cy - h / 2), (cx + w / 2, cy - h / 4), (cx + w / 2, cy + h / 4),
           (cx, cy + h / 2), (cx - w / 2, cy + h / 4), (cx - w / 2, cy - h / 4)]
    return "M " + " L ".join(f"{x:.2f},{y:.2f}" for x, y in pts) + " Z"


def sine(harmonic=1, amp=AMP, phase=0.0, n=400):
    """Sinus der Ordnung `harmonic` ueber PERIODS Perioden der Grundschwingung."""
    pts = []
    for i in range(n + 1):
        x = X0 + (X1 - X0) * i / n
        t = (x - X0) / (X1 - X0) * 2 * math.pi * PERIODS
        v = math.sin(harmonic * t + math.radians(phase))
        pts.append((x, Y0 - v * amp))
    return "M " + " L ".join(f"{a:.1f},{b:.1f}" for a, b in pts)


def trace(d, col, w=3.4, op=1.0):
    return (f'<path d="{d}" fill="none" stroke="{col}" stroke-width="{w}" '
            f'stroke-opacity="{op}" stroke-linecap="round" stroke-linejoin="round"/>')


def graticule():
    out = [f'<g stroke="{PAL["grid"]}" stroke-opacity="0.30" stroke-width="1" fill="none">']
    x = -104.0
    while x <= 104.001:
        out.append(f'<path d="M {x:.1f},-125 L {x:.1f},125"/>')
        x += 26.0
    y = -120.0
    while y <= 120.001:
        out.append(f'<path d="M -110,{y:.1f} L 110,{y:.1f}"/>')
        y += 26.0
    out.append('</g>')
    # Nulllinie mit Teilstrichen
    out.append(f'<g stroke="{PAL["grid"]}" stroke-opacity="0.75" stroke-width="1.6" '
               'stroke-linecap="round">')
    out.append(f'<path d="M -100,{Y0} L 100,{Y0}"/>')
    v = -87.0
    while v < 99.99:
        out.append(f'<path d="M {v:.1f},{Y0 - 3.5} L {v:.1f},{Y0 + 3.5}"/>')
        v += 13.0
    out.append('</g>')
    return "\n".join(out)


def glow_trace(d, col, w, glow):
    """Kurve mit weichem Leuchtrand: breitere, weichgezeichnete Linie darunter."""
    gcol, gw, gop = glow
    return (f'<path d="{d}" fill="none" stroke="{gcol}" stroke-width="{w + gw}" '
            f'stroke-opacity="{gop}" stroke-linecap="round" stroke-linejoin="round" '
            f'filter="url(#pqdmGlow)"/>\n    ' + trace(d, col, w=w))


def logo(width=240, title=TITLE, tight=True):
    """tight=True: viewBox schliesst exakt mit der Hexagon-Kante ab (240 x 277 px),
    das ist das uebliche Seitenverhaeltnis fuer man/figures/logo.png."""
    H = W * 2 / math.sqrt(3)
    box = (f'{-W/2:.2f} {-H/2:.2f} {W:.2f} {H:.2f}' if tight else '-110 -125 220 250')
    ring = (f'<path d="{hex_path(0, 0, W)} {hex_path(0, 0, W - 2 * BORDER_W)}" '
            f'fill="{PAL["border"]}" fill-rule="evenodd"/>')
    txt = ""
    if title:
        txt = (f'<text x="{TITLE_LS / 2:.1f}" y="{TITLE_Y}" text-anchor="middle" '
               f'font-family="{FONT}" font-size="{TITLE_SIZE}" font-weight="700" '
               f'letter-spacing="{TITLE_LS}" fill="{PAL["title"]}">{title}</text>')
        # letter-spacing haengt hinter dem letzten Buchstaben; x = ls/2 zentriert optisch
    return f'''<svg viewBox="{box}" width="{width}" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <clipPath id="pqdmHex"><path d="{hex_path(0, 0, W)}"/></clipPath>
    <filter id="pqdmGlow" x="-20%" y="-50%" width="140%" height="200%">
      <feGaussianBlur stdDeviation="{BLUR}"/>
    </filter>
  </defs>
  <g clip-path="url(#pqdmHex)">
    <path d="{hex_path(0, 0, W)}" fill="{PAL["bg"]}"/>
{graticule()}
    {glow_trace(sine(3, AMP * H3, PH3), PAL["sine"], H3_W, H3_GLOW)}
    {glow_trace(sine(), PAL["lead"], FUND_W, FUND_GLOW)}
    {ring}
  </g>
  {txt}
</svg>'''


if __name__ == "__main__":
    width = int(sys.argv[1]) if len(sys.argv) > 1 else 240
    if len(sys.argv) > 2:                  # python make_logo.py 240 orange
        PAL = palette(sys.argv[2])
    sys.stdout.write(logo(width))
