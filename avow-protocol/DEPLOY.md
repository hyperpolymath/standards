# Deploy STAMP Protocol Website to Cloudflare Pages

## ✅ What's Ready

- Professional landing page
- Mobile responsive
- SEO optimized
- Links to your Telegram bot (@stamp_demo_bot)
- Ready to deploy

## 🚀 Deploy Now (5 Minutes)

### Option 1: GitHub + Cloudflare Pages (Recommended)

**1. Push to GitHub:**
```bash
cd ~/Documents/hyperpolymath-repos/stamp-protocol

# Create GitHub repo
gh repo create stamp-protocol --public --source=. --remote=origin --push

# Or manually:
git remote add origin https://github.com/hyperpolymath/stamp-protocol.git
git push -u origin main
```

**2. Connect to Cloudflare Pages:**

1. Go to https://dash.cloudflare.com/
2. Click **Pages** in the sidebar
3. Click **Create a project**
4. Click **Connect to Git**
5. Authorize GitHub
6. Select **stamp-protocol** repository
7. Configure build settings:
   - **Project name:** stamp-protocol
   - **Production branch:** main
   - **Framework preset:** None
   - **Build command:** (leave empty)
   - **Build output directory:** `/`
8. Click **Save and Deploy**

**3. Configure Custom Domain:**

1. After deploy, go to **Custom domains**
2. Click **Set up a custom domain**
3. Enter: `stamp-protocol.org`
4. Cloudflare will auto-configure DNS
5. SSL certificate issued automatically

**Done!** Your site will be live at https://stamp-protocol.org in ~2 minutes.

### Option 2: Direct Upload (Faster for Testing)

```bash
# Install Wrangler
npm install -g wrangler

# Login
wrangler login

# Deploy
cd ~/Documents/hyperpolymath-repos/stamp-protocol
wrangler pages deploy . --project-name=stamp-protocol
```

Then configure domain in Cloudflare dashboard.

## 🔄 Updates

After first deploy, just push to GitHub:
```bash
git add .
git commit -m "Update content"
git push
```

Cloudflare automatically rebuilds and deploys in ~30 seconds.

## 🌐 Custom Domain Setup

If you need to manually configure DNS:

1. Go to **DNS** in Cloudflare dashboard
2. Add CNAME record:
   - **Name:** `@`
   - **Target:** `stamp-protocol.pages.dev`
   - **Proxy status:** Proxied (orange cloud)
3. Save

SSL certificate will be issued automatically.

## ✅ Verify Deployment

Visit: https://stamp-protocol.org

Check:
- [ ] Homepage loads
- [ ] Mobile responsive
- [ ] Telegram bot link works
- [ ] All sections present
- [ ] SSL certificate valid

## 📊 Add Analytics (Optional)

1. Go to Cloudflare Pages dashboard
2. Click **Web Analytics**
3. Copy the beacon code
4. Add to `index.html` before `</body>`
5. Push update

## 🎯 What's on the Site

- **Hero:** Value proposition + CTA to Telegram bot
- **Problem:** Email spam, social bots, platform costs
- **Solution:** Dependent types + formal verification
- **How it works:** Technical comparison
- **Demo:** Telegram bot walkthrough
- **Use cases:** Email, dating, social media, business
- **Stats:** Impact metrics
- **Footer:** Links and contact

Ready to show investors and users!
