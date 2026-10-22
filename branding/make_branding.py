"""Build the hex sticker, favicons, and link-preview image for the two packages
(simstreams in R, simstreams in Python) from one design: parallel streams.

  uv run --with fonttools --with pillow python branding/make_branding.py   (from this repo's root)

The design (Chester's pick from the October 22, 2026 proof sheet: B, G, L):
four braided streams leave one seed and pool at the end; the tab icon is a
hexagon with three of the streams; the link preview carries a tall tab at
the left edge after the book series' cover. The R sticker uses the book's
navy and magenta; the Python sticker a dark plum with a gold rim.

Writes the sources and renders here, then copies what each package serves:
  branding/                        logo.svg, logo.png, favicon.svg, favicon-32.png, favicon-96x96.png,
                                   favicon.ico, apple-touch-icon.png, og-source.html, og.png, site.webmanifest
  man/figures/                     logo.svg, logo.png            (README, pkgdown navbar)
  ../simstreams-python/branding/  the same set for the Python package, when that checkout exists

The letters in the logo are outlines of Big Shoulders Display Black (SIL Open
Font License), so the files look the same on any machine. The font file is
downloaded once into branding/.cache and is not committed. The link-preview
image is rendered from og-source.html by headless Chrome through
~/.claude/skills/web-page-meta/render_meta.py.
"""
import json
import math
import pathlib
import shutil
import subprocess
import sys
import tempfile
import urllib.request

from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen
from fontTools.ttLib import TTFont

HERE = pathlib.Path(__file__).resolve().parent
PROJECT = HERE.parent
PY_PROJECT = PROJECT.parent / "simstreams-python"
FONT_URL = ("https://fonts.gstatic.com/s/bigshouldersdisplay/v24/"
            "fC1MPZJEZG-e9gHhdI4-NBbfd2ys3SjJCx12wPgf9g-_3F0YdSY8JF4.ttf")
FONT_PATH = HERE / ".cache" / "BigShouldersDisplay-Black.ttf"
CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
RENDER_META = pathlib.Path.home() / ".claude/skills/web-page-meta/render_meta.py"

CREAM = "#f4efe0"
MAGENTA = "#a50162"   # the book's series tab color
GOLD = "#f2c14e"
TEAL = "#4fb3bf"
ORANGE = "#f08a3c"
PINK = "#e8679a"
PKGS = {
    "r": dict(name="simstreams", kind="R package", bg="#02172c", rim=MAGENTA,
              lane="#1e3a55", chip="R", chip_bg=MAGENTA, chip_fg=CREAM,
              url="github.com/ismayc/simstreams",
              sub="Parallel simulation experiments on a personal computer: one independent PCG64 stream per process, the worker as an argument",
              title_px=118),
    "py": dict(name="simstreams", kind="Python package", bg="#2b1237", rim=GOLD,
               lane="#4a2a58", chip="py", chip_bg=GOLD, chip_fg="#2b1237",
               url="github.com/ismayc/simstreams-python",
               sub="Parallel simulation experiments on a personal computer: spawned SeedSequence streams, the worker as an argument",
               title_px=118),
}
HEX = "100,4.5 196.2,60 196.2,171 100,226.5 3.8,171 3.8,60"
HEX_INNER = "100,13 188.5,64.5 188.5,166.5 100,218 11.5,166.5 11.5,64.5"
COLORS = [TEAL, ORANGE, PINK, GOLD]  # one per stream


def load_font():
    if not FONT_PATH.exists():
        FONT_PATH.parent.mkdir(exist_ok=True)
        urllib.request.urlretrieve(FONT_URL, FONT_PATH)
    return TTFont(FONT_PATH)


FONT = None


def text_path(text, size, cx, baseline, fill, width=None):
    """Outline `text` centered on cx. With `width`, stretch it to that width."""
    global FONT
    FONT = FONT or load_font()
    gs, cmap, upm = FONT.getGlyphSet(), FONT.getBestCmap(), FONT["head"].unitsPerEm
    names = [cmap[ord(c)] for c in text]
    advance = sum(gs[n].width for n in names)
    sy = size / upm
    sx = (width / advance) if width else sy
    x = cx - advance * sx / 2
    pen = SVGPathPen(gs)
    for n in names:
        gs[n].draw(TransformPen(pen, (sx, 0, 0, -sy, x, baseline)))
        x += gs[n].width * sx
    return f'<path d="{pen.getCommands()}" fill="{fill}"/>'


def chip(cx, cy, w, h, label, bg, fg):
    return (f'<rect x="{cx - w/2}" y="{cy - h/2}" width="{w}" height="{h}" rx="{h/2}" fill="{bg}"/>'
            + text_path(label, h * 0.62, cx, cy + h * 0.3, fg))


def wave(x0, x1, cy, amp, phase, cycles, color, width, n=48):
    """One stream: a sine wave pinched to a point at both ends."""
    pts = []
    for i in range(n + 1):
        t = i / n
        x = x0 + (x1 - x0) * t
        y = cy + amp * math.sin(math.pi * t) * math.sin(2 * math.pi * cycles * t + phase)
        pts.append(f"{x:.1f},{y:.1f}")
    return (f'<polyline points="{" ".join(pts)}" fill="none" stroke="{color}" '
            f'stroke-width="{width}" stroke-linecap="round" stroke-linejoin="round"/>')


def braid(p):
    """Four streams leave one seed, run side by side, and pool at the end."""
    out = [f'<circle cx="34" cy="91" r="6" fill="{CREAM}"/>', f'<circle cx="166" cy="91" r="6" fill="{CREAM}"/>']
    for k, color in enumerate(COLORS):
        out.append(wave(40, 160, 91, 16 + 4 * k, k * math.pi / 2, 1.5, color, 4))
    return "".join(out)


def logo_svg(p):
    """The hex sticker: four braided streams between a seed and a pooled result."""
    return (
        '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 231" width="400" height="462" '
        f'role="img" aria-label="{p["name"]} hex sticker: four braided random-number streams">'
        f'<polygon points="{HEX}" fill="{p["bg"]}"/>'
        f'<polygon points="{HEX}" fill="none" stroke="{p["rim"]}" stroke-width="7" stroke-linejoin="round"/>'
        f'<polygon points="{HEX_INNER}" fill="none" stroke="{p["rim"]}" stroke-width="1.2" stroke-dasharray="3 4" opacity=".7"/>'
        + braid(p)
        + text_path("simstreams", 36, 100, 170, CREAM, width=150)
        + chip(100, 199, 38 if p["chip"] == "py" else 34, 18, p["chip"], p["chip_bg"], p["chip_fg"])
        + "</svg>"
    )


def favicon_svg(p):
    """A hexagon, like the sticker, holding three braided streams."""
    mark = [f'<circle cx="9" cy="32" r="3.5" fill="{CREAM}"/>', f'<circle cx="55" cy="32" r="3.5" fill="{CREAM}"/>']
    for k, color in enumerate([TEAL, ORANGE, PINK]):
        mark.append(wave(12, 52, 32, 9 + 3 * k, k * 2 * math.pi / 3, 1.5, color, 3.2, n=32))
    return (
        '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64" width="64" height="64">'
        f'<polygon points="32,2 59,17 59,47 32,62 5,47 5,17" fill="{p["bg"]}" stroke="{p["rim"]}" '
        'stroke-width="3" stroke-linejoin="round"/>'
        f'<g transform="translate(32 32) scale(.78) translate(-32 -32)">{"".join(mark)}</g></svg>'
    )


def og_html(p, hex_svg):
    """Series-tab layout: a tall tab at the left edge, after the book series'
    cover tab, the name set large, one line, and a small sticker."""
    inline = hex_svg.replace('width="400" height="462"', 'width="210" height="243"')
    tab_fg = p["bg"] if p["chip"] == "py" else CREAM
    return f"""<!doctype html>
<!-- Link-preview source for {p['name']} ({p['kind']}): 1200x630.
     Re-render: uv run --with fonttools --with pillow python branding/make_branding.py
     (writes og.png with ~/.claude/skills/web-page-meta/render_meta.py) -->
<html lang="en"><head><meta charset="utf-8">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link href="https://fonts.googleapis.com/css2?family=Big+Shoulders+Display:wght@900&family=Barlow:wght@400;600&display=swap" rel="stylesheet">
<style>
  html, body {{ width: 1200px; height: 630px; margin: 0; overflow: hidden; }}
  body {{ background: {p['bg']}; color: {CREAM}; display: grid; grid-template-columns: 120px 1fr;
          font-family: 'Barlow', 'Helvetica Neue', Arial, sans-serif; }}
  .tab {{ background: {p['rim']}; color: {tab_fg}; display: flex; align-items: flex-end;
          justify-content: center; padding-bottom: 48px; }}
  .tab span {{ writing-mode: vertical-rl; transform: rotate(180deg); font-size: 34px; font-weight: 600;
               letter-spacing: .06em; }}
  .main {{ position: relative; padding: 72px 72px 0 88px; }}
  .title {{ font: 900 150px/.85 'Big Shoulders Display', 'Arial Narrow', Impact, sans-serif;
            white-space: nowrap; }}
  .sub {{ font-size: 36px; line-height: 1.3; max-width: 640px; margin-top: 22px; opacity: .92; }}
  .url {{ position: absolute; left: 88px; bottom: 56px; font-size: 28px; font-weight: 600; opacity: .75; }}
  .sticker {{ position: absolute; right: 64px; bottom: 40px; }}
  svg {{ display: block; }}
</style></head><body>
<div class="tab"><span>{p['kind']}</span></div>
<div class="main"><div class="title">{p['name']}</div><div class="sub">{p['sub']}</div>
<div class="url">{p['url']}</div><div class="sticker">{inline}</div></div>
</body></html>
"""


def chrome_shot(url, out, w, h, transparent=False):
    out.unlink(missing_ok=True)
    flags = ["--default-background-color=00000000"] if transparent else []
    with tempfile.TemporaryDirectory() as prof:
        subprocess.run(
            ["perl", "-e", "alarm 60; exec @ARGV", CHROME, "--headless=new", "--disable-gpu",
             "--hide-scrollbars", f"--user-data-dir={prof}", f"--window-size={w},{h}",
             "--virtual-time-budget=5000", *flags, f"--screenshot={out}", url],
            check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if not out.exists():
        sys.exit(f"render failed: {out}")


def build(key, brand_dir, copies):
    p = PKGS[key]
    brand_dir.mkdir(parents=True, exist_ok=True)
    logo = logo_svg(p)
    (brand_dir / "logo.svg").write_text(logo)
    (brand_dir / "favicon.svg").write_text(favicon_svg(p))
    (brand_dir / "og-source.html").write_text(og_html(p, logo))
    subprocess.run([sys.executable, "-I", str(RENDER_META), "--dir", str(brand_dir), "--bg", p["bg"]],
                   check=True)
    # logo.png: transparent corners, 2x of the 200-unit viewBox
    wrap = brand_dir / ".logo-wrap.html"
    wrap.write_text(f'<!doctype html><html><body style="margin:0;background:transparent">{logo}</body></html>')
    chrome_shot(f"file://{wrap}", brand_dir / "logo.png", 400, 462, transparent=True)
    wrap.unlink()
    # favicon.ico for browsers that ask for /favicon.ico: 16, 32, and 48 px
    from PIL import Image
    Image.open(brand_dir / "apple-touch-icon.png").save(
        brand_dir / "favicon.ico", sizes=[(16, 16), (32, 32), (48, 48)])
    Image.open(brand_dir / "apple-touch-icon.png").resize((96, 96), Image.LANCZOS).save(
        brand_dir / "favicon-96x96.png")
    (brand_dir / "site.webmanifest").write_text(json.dumps({
        "name": p["name"], "short_name": p["name"],
        "icons": [{"src": "favicon-96x96.png", "sizes": "96x96", "type": "image/png"},
                  {"src": "apple-touch-icon.png", "sizes": "180x180", "type": "image/png"}],
        "theme_color": p["bg"], "background_color": "#fafbf7", "display": "standalone"}, indent=2) + "\n")
    for src, dest in copies:
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy(brand_dir / src, dest)


def main():
    fav = PROJECT / "pkgdown" / "favicon"
    build("r", HERE, [
        ("logo.svg", PROJECT / "man" / "figures" / "logo.svg"),
        ("logo.png", PROJECT / "man" / "figures" / "logo.png"),
        # pkgdown copies pkgdown/favicon/ to the site root and then does not
        # build its own favicons (which would call an online service)
        ("favicon.svg", fav / "favicon.svg"),
        ("favicon-32.png", fav / "favicon-32.png"),
        ("favicon-96x96.png", fav / "favicon-96x96.png"),
        ("apple-touch-icon.png", fav / "apple-touch-icon.png"),
        ("favicon.ico", fav / "favicon.ico"),
        ("site.webmanifest", fav / "site.webmanifest"),
        # pkgdown/assets/ is copied to the site root too: the link preview
        ("og.png", PROJECT / "pkgdown" / "assets" / "og.png"),
    ])
    print(f"wrote R branding to {HERE}")
    if PY_PROJECT.exists():
        docs = PY_PROJECT / "docs"
        build("py", PY_PROJECT / "branding", [
            # the mkdocs site: header logo, favicons, link preview, and the
            # README's sticker path (branding/logo.png) resolved from docs/
            ("logo.svg", docs / "assets" / "logo.svg"),
            ("favicon.svg", docs / "assets" / "favicon.svg"),
            ("favicon-32.png", docs / "assets" / "favicon-32.png"),
            ("apple-touch-icon.png", docs / "assets" / "apple-touch-icon.png"),
            ("og.png", docs / "assets" / "og.png"),
            ("logo.png", docs / "branding" / "logo.png"),
        ])
        print(f"wrote Python branding to {PY_PROJECT / 'branding'}")
    else:
        print(f"skipped Python branding: {PY_PROJECT} not found")


if __name__ == "__main__":
    main()
