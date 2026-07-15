# CLAUDE.md

## TI4 Tournament VOD Recap Pipeline

Recurring task: turn a downloaded Twilight Imperium 4 tournament VOD (SCPT
Thunder's Edge Patreon tournament, ~7h webm in `~/Downloads`) into a polished
static HTML recap with embedded screenshots. First one produced:
`~/Downloads/ti4-semifinal-recap.html` ("A Crimson Wind", July 2026).

### Inputs

- Video: `~/Downloads/*.webm`, filename ends with the YouTube ID in brackets,
  e.g. `[EXi_CJ77RJA]`. **Never re-download the video** — it's already local.
- Duration/size via `ffprobe -v error -show_entries format=duration,size -of
  default=noprint_wrappers=1 "$V"`.

### Step 1 — Transcript (do this first; it's cheap and high-signal)

Fetch auto-captions only (a few MB, not the video — user has approved this):

```sh
yt-dlp --skip-download --write-auto-sub --sub-lang en --sub-format vtt \
  -o ti4 "https://www.youtube.com/watch?v=<ID>"
```

Auto-caption VTTs have massive duplication from rolling display. Clean into
`[h:mm] text` lines by dropping timestamps/`<c>` tags and deduping consecutive
identical lines (parse `(\d+):(\d+):(\d+)\.\d+ -->` for the timestamp; keep
h:mm granularity). ~7h game ≈ 80k words — do NOT read linearly. Read:

- **First ~120 lines**: commentator, tournament context, player intros with
  seat colors, and the faction draft banter.
- **Last ~250 lines**: the winning move, "that's game", congratulations, and
  the post-mortem ("how I could have won" phase) — great quote material.
- **Grep for pivots**: `custodian`, `imperial`, `wins the game|congratulations`,
  `support` (Support for the Throne politics), agenda names, `war sun`,
  faction/player names, `crimson|rebellion` (or whatever factions are in play).
  Read ±20-line windows around hits.

Transcription quirks: "Space Cats Peace Turtles" garbles ("Beast Turtles");
player names mangle (Ultra Gainer → "Ultra Gator"); "Mecatol Rex" → "Meatl
Rex"; "guac" is SCPT slang. Draft-lobby seat colors ≠ final in-game colors.

### Step 2 — Frames

```sh
V=~/Downloads/<file>.webm
mkdir -p frames
for m in $(seq 0 15 <duration_min>); do
  ffmpeg -v error -ss $((m*60)) -i "$V" -frames:v 1 -vf scale=1568:-1 \
    -y frames/m$(printf %03d $m).jpg
done
```

`-ss` before `-i` (fast seek). 15-min spacing suffices for the arc; add extra
frames near the end (last ~10 min) for the winning moment, and densify
anywhere the scores jump between samples. Stream intro lasts ~20 min — m000
is usually a title card.

**Reading a TTS/SCPT frame** (Read tool on the jpg):
- **Top left**: round number + the active player's faction and researched techs.
- **Top right**: score panel — player rows with faction icon, name, strategy
  card (struck through = played), and VP. This is the ground truth for scores.
- **Right side**: public objectives with per-player progress grids.
- **Bottom left**: game log/chat — combat results, "X recycled: …",
  "AGENDA … results", whispers. Very high signal for what just happened.
- Player color mapping comes from log lines like "red recycled: Command
  (Rebellion)" cross-referenced with the score panel faction icons.

### Step 3 — Reconcile and go deep (HTML gives room for detail)

Build a per-round score table from the frames (VP per player per sampled
timestamp). For each score jump, find the cause in transcript/log. Track:
custodians timing, agenda outcomes and vote blocs, Supports for the Throne,
the Imperial holder per round, key combats (grep the frame chat for
"hits from"/"gains 1 point"), secrets scored, and the tech story (what each
player researched and whether commentary praised/roasted it — pull verbatim
quotes). For the HTML version also capture: the draft (who wanted what),
2–3 commentary quotes per act, and the exact final sequence blow-by-blow.

### Step 4 — Build the HTML

Pattern: `recap_template.html` with `{{IMG_KEY}}` placeholders + a small
python build script that base64-inlines chosen frames as
`data:image/jpeg;base64,…` and writes the final file. 5–8 screenshots ≈
600KB–1MB total — fine. Output goes to `~/Downloads/<event>-recap.html`,
then `DISPLAY=:0 xdg-open` it.

**Do not use the Artifact tool for this** — it failed repeatedly with DNS
timeouts from the tool's own network path (host DNS was fine). Local
self-contained HTML is the deliverable; it's portable/shareable as-is.

Established design language (keep consistent across game recaps so they read
as a series):
- Committed dark single theme (deep space — deliberate, no light mode).
- Tokens: ground `#0A0D18`, panel `#131A2C`, text `#C9D2E4`, headings
  `#EEF2FA`, dim `#7D889F`, hairline `#26304A`, accent gold `#E3B34C`
  (custodians/throne), crimson `#D8434A` reserved for the winner.
- Type: display = Futura/Avenir Next/Century Gothic/Trebuchet, uppercase,
  tracked wide (game-box titling); body = Iowan Old Style/Palatino/Georgia
  serif (chronicle voice); scores/data = ui-monospace with tabular-nums.
- Layout: ~68ch chronicle column inside a 980px wrap; screenshots break out
  to full wrap width as `<figure>` with gold-tagged captions ("ROUND 4 · …");
  round timeline uses a two-column grid (gold round marker | body); final
  standings in a bordered panel with the winner's row in crimson; tech notes
  as left-ruled blocks; one centered pull-quote from the endgame.
- Subtle CSS starfield: stacked 1px radial-gradients on the body background.
- Title pattern: evocative two-word headline (e.g. "A Crimson Wind") +
  eyebrow "Space Cats Peace Turtles · <event>". No `<!doctype>`/`<html>`
  wrapper needed if publishing via Artifact, but for local files include a
  minimal `<!doctype html><meta charset="utf-8"><meta name="viewport"
  content="width=device-width,initial-scale=1">` prefix.

### Structure of the recap page

1. Masthead: eyebrow / headline / one-sentence deck / hairline rule.
2. Context paragraph (tournament, commentator, draft highlights).
3. Roster table with faction color swatches.
4. Opening-board screenshot.
5. Round-by-round chronicle interleaved with screenshots at pivots.
6. Endgame screenshot pair (match point + winning moment) + pull quote.
7. Final standings panel.
8. "The tech that decided it" section (3–4 entries, quotes included).
9. One-sentence "story of the game" closer + footer (source/method note).

### Misc

- Work in the session scratchpad; only the final HTML goes to `~/Downloads`.
- zsh gotchas: quote heredoc delimiters (`<<'EOF'`); `echo ===` fails
  (parsed as glob) — quote it.
- Game 2 VOD is already downloaded: `SCPT 2026 Semis Game 2 [S0FzsIkG3Ac].webm`.
