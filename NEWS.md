# rayimage 0.28.0

## New features

- `render_tagline()` Adds small corner credits, automatic sizing, configurable padding and opacity, and bottom-right placement by default.

## Bugfixes

- `render_tonemap()` Returns linear pixels for the `"hbd"` method, preventing display encoding from being applied twice when plotting or saving images.
