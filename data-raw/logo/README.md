# Logo

`make_logo.py` erzeugt das Hexlogo als SVG; daraus entstehen die PNG-Fassungen.

```
python make_logo.py 240 > logo.svg
rsvg-convert -w 240 -o ../../man/figures/logo.png logo.svg
```

Ohne `rsvg-convert` geht der Export auch mit Chrome im Headless-Modus. Dazu
`logo.svg` in eine HTML-Seite mit transparentem Hintergrund einbinden
(`<img>` mit 240 x 277.13 px, ohne Rand) und fotografieren:

```
chrome --headless=new --hide-scrollbars --default-background-color=00000000 \
  --allow-file-access-from-files --window-size=240,277 \
  --force-device-scale-factor=1 --screenshot=logo.png logo_page.html
```

`--force-device-scale-factor=2` ergibt `logo-2x.png`, `5` ergibt
`logo-large.png`. Der Weichzeichner der Leuchtraender (SVG-Filter) wird dabei
fest eingerechnet.

Die Datei `../../man/figures/logo.png` (240 x 277 px, transparenter Hintergrund)
ist die im Paket verwendete Fassung. `logo-2x.png` und `logo-large.png` sind
Exporte derselben Vorlage fuer Praesentationen und Druck.

Motiv und Parameter sind in `make_logo.py` dokumentiert.

## Farbfamilie

Alle Farben haben dieselbe Helligkeit und Saettigung (OKLCH), nur der Farbton
wechselt. Grundschwingung und Schrift sind weiss; Raster und 3. Harmonische
sind dunkler als der Grund. Die Leuchtraender (dunkel um die Grundschwingung,
weiss um die Harmonische) sind fuer alle Farben gleich.

| Farbe  | Verwendung           | Grund     | Rand      | Linien    |
|--------|----------------------|-----------|-----------|-----------|
| petrol | PQDM                 | `#0099ae` | `#007f92` | `#005f6e` |
| berry  | reserviert           | `#bd658a` | `#9f5172` | `#783c55` |
| brick  | reserviert           | `#c4685a` | `#a55347` | `#7d3d34` |
| orange | reserviert           | `#bf6e3e` | `#a1592e` | `#794220` |
| purple | reserviert           | `#9772c0` | `#7e5da2` | `#5e457a` |

```
python make_logo.py 240 orange > orange.svg
```
