# STAMP Protocol Demo Video Script

**Duration:** 2 minutes
**Goal:** Show investors/users what STAMP does and why it matters

## 🎬 Recording Setup

### Software Options

**Linux (Fedora):**
```bash
# Option 1: SimpleScreenRecorder (best quality)
sudo dnf install simplescreenrecorder
simplescreenrecorder

# Option 2: OBS Studio (professional)
flatpak install flathub com.obsproject.Studio
flatpak run com.obsproject.Studio

# Option 3: FFmpeg (command line)
ffmpeg -video_size 1920x1080 -framerate 30 -f x11grab -i :0.0 \
  -c:v libx264 -preset ultrafast -crf 23 stamp-demo.mp4
```

**Settings:**
- Resolution: 1920x1080 (1080p)
- Frame rate: 30fps
- Audio: Optional (voiceover or silent)
- Format: MP4 (H.264)

### What to Show

1. **Website** (stamp-protocol.org)
2. **Telegram bot** (@stamp_demo_bot)
3. **Interactive demo** (verification in browser)

## 📝 Script

### Scene 1: Hook (0:00-0:15)

**Visual:** stamp-protocol.org homepage

**Narration:**
> "Email spam. Fake social media accounts. Broken unsubscribe links.
>
> What if you could **mathematically prove** your messages comply with
> consent, working unsubscribe, and rate limits?
>
> That's STAMP Protocol."

**On-screen text:**
- "80-90% bot reduction"
- "Mathematically proven compliance"

---

### Scene 2: The Problem (0:15-0:35)

**Visual:** Scroll to "Problem" section

**Narration:**
> "Current solutions rely on promises. Developers can lie about testing
> unsubscribe links. Spammers bypass rate limits.
>
> Platforms spend over $100 million per year fighting bots—and still lose."

**On-screen text:**
- "$200M+ email spam market"
- "$1.2B+ social media bot market"

---

### Scene 3: The Solution (0:35-1:00)

**Visual:** "How It Works" section, show code comparison

**Narration:**
> "STAMP uses dependent types in Idris2 to prove message properties
> at compile-time.
>
> Without STAMP, developers can lie. With STAMP, the code literally
> won't compile if your unsubscribe link doesn't work.
>
> It's not testing. It's proof."

**On-screen text:**
- "Dependent Types = Mathematical Proof"
- "Code won't compile if invalid"

---

### Scene 4: Live Demo (1:00-1:40)

**Visual:** Switch to Telegram, show bot interaction

**Narration:**
> "Here's STAMP in action. I'll subscribe to the demo bot.
>
> [Type /start]
>
> Immediately, I get a cryptographic proof of my consent—with
> timestamps that can't be faked.
>
> [Type /verify]
>
> The bot shows the unsubscribe link was tested and proven to work
> in under 200 milliseconds.
>
> [Type /unsubscribe]
>
> One click. Proven removal. No dark patterns."

**Key moments to capture:**
- `/start` response (consent proof)
- `/verify` output (cryptographic proof)
- `/unsubscribe` confirmation

---

### Scene 5: Call to Action (1:40-2:00)

**Visual:** Back to website, hero section

**Narration:**
> "STAMP works for email, social media, business messaging—any
> protocol that needs verified consent.
>
> Reduce bots by 80-90%. Save millions on moderation. Prove
> regulatory compliance.
>
> Try the live demo at stamp-protocol.org or test the Telegram bot.
>
> STAMP Protocol: Spam is over."

**On-screen text:**
- "stamp-protocol.org"
- "@stamp_demo_bot"
- "Open source. AGPL-3.0"

---

## 🎥 Recording Checklist

**Before recording:**
- [ ] Close unnecessary tabs/apps
- [ ] Set browser zoom to 100%
- [ ] Hide bookmarks bar
- [ ] Clear Telegram chat (or use test account)
- [ ] Test bot commands work
- [ ] Restart bot if needed
- [ ] Practice script 2-3 times

**Recording settings:**
- [ ] 1920x1080 resolution
- [ ] 30fps framerate
- [ ] Clean desktop (no clutter)
- [ ] Good lighting (if showing webcam)
- [ ] Clear audio (if narrating)

**Post-recording:**
- [ ] Trim beginning/end
- [ ] Add title card (optional)
- [ ] Add captions (accessibility)
- [ ] Export as MP4
- [ ] Test playback
- [ ] Upload to YouTube/Vimeo

## 📤 Where to Share

1. **YouTube** (unlisted or public)
2. **Vimeo** (good for business)
3. **Twitter/X** (2-min native video)
4. **LinkedIn** (for B2B)
5. **Embed on website**

## 🎬 Quick Recording Commands

### Using SimpleScreenRecorder

1. Open SimpleScreenRecorder
2. Select area: "Record entire screen"
3. Audio: Optional
4. Output file: `~/Videos/stamp-demo.mp4`
5. Click "Record"
6. Do your demo
7. Click "Stop"

### Using OBS

1. Open OBS Studio
2. Scene: "Screen Capture"
3. Start Recording
4. Do your demo
5. Stop Recording
6. File saved in `~/Videos/`

### Quick FFmpeg (Command Line)

```bash
# Start recording
ffmpeg -video_size 1920x1080 -framerate 30 -f x11grab -i :0.0+0,0 \
  -c:v libx264 -preset ultrafast -crf 18 \
  ~/Videos/stamp-demo-$(date +%Y%m%d).mp4

# Stop with Ctrl+C

# Add voiceover later (optional)
ffmpeg -i stamp-demo.mp4 -i audio.mp3 -c:v copy -c:a aac \
  stamp-demo-final.mp4
```

## ✂️ Editing (Optional)

**Quick edits with FFmpeg:**

```bash
# Trim first 5 seconds
ffmpeg -ss 00:00:05 -i input.mp4 -c copy output.mp4

# Trim last 3 seconds
ffmpeg -i input.mp4 -t $(echo "$(ffprobe -v error -show_entries \
  format=duration -of default=noprint_wrappers=1:nokey=1 input.mp4) - 3" | bc) \
  -c copy output.mp4

# Add fade in/out
ffmpeg -i input.mp4 -vf "fade=in:0:30,fade=out:3570:30" output.mp4

# Speed up 1.25x (if too slow)
ffmpeg -i input.mp4 -filter:v "setpts=0.8*PTS" output.mp4
```

**Or use:**
- Kdenlive (open source, powerful)
- Shotcut (simpler)
- DaVinci Resolve (professional, free)

## 📊 Success Metrics

**Good demo video:**
- Under 2 minutes ✓
- Shows problem → solution → demo
- Clear audio/visual
- Sharable link

**Great demo video:**
- Professional editing
- Captions/subtitles
- Engaging narration
- Call to action

## 🚀 Ready to Record?

1. Install screen recorder
2. Practice script
3. Record (multiple takes OK)
4. Pick best take
5. Upload and share!

**Quick start:**
```bash
# Install SimpleScreenRecorder
sudo dnf install simplescreenrecorder

# Launch
simplescreenrecorder &

# OR use OBS
flatpak run com.obsproject.Studio &
```

Remember: **Done is better than perfect.** Even a simple screen recording
with no audio is valuable for showing the demo to people.
