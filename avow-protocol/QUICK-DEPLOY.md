# Quick Cloudflare Deployment (5 Minutes)

## Step 1: Connect Repository (2 minutes)

1. Go to: https://dash.cloudflare.com/pages
2. Click **"Create a project"**
3. Click **"Connect to Git"**
4. Select **"GitHub"**
5. Find and select: **`hyperpolymath/avow-protocol`**
6. Click **"Begin setup"**

## Step 2: Configure Build (1 minute)

**Build settings:**
- Framework preset: **None**
- Build command: **`deno task build`**
- Build output directory: **`.`** (just a dot)
- Root directory: **`/`** (leave as default)

Click **"Save and Deploy"**

## Step 3: Watch Build (2 minutes)

- Build starts automatically
- Takes 2-3 minutes
- Shows real-time logs
- Look for: ✅ Success!

## Step 4: Get Your URL

Your site will be live at:
- **`https://avow-protocol.pages.dev`**

## Step 5: Add Custom Domain (Optional)

1. In your project, click **"Custom domains"**
2. Click **"Set up a custom domain"**
3. Enter: **`avow-protocol.org`**
4. Cloudflare auto-configures DNS
5. Wait 1-5 minutes for activation

## That's It! 🎉

Your AVOW Protocol site is now live with:
- ✅ Global CDN
- ✅ Auto SSL/TLS
- ✅ Auto-deploy on git push
- ✅ Post-quantum TLS 1.3
- ✅ DDoS protection

Visit your site and verify it works!

## Next: Complete Security Setup

Follow **CLOUDFLARE-MANUAL-SETUP.md** for:
- Security headers
- WAF rules
- Zero Trust
- Monitoring
