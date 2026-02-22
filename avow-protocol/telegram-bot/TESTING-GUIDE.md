# STAMP Bot Testing Guide

## 🎯 Goal
Get 3-5 people to test @stamp_demo_bot and provide feedback on:
- Ease of use
- Clarity of proofs
- Understanding of STAMP value proposition
- Any bugs or confusion

## 👥 Who to Ask

**Best testers:**
- Tech-savvy friends (understand proofs)
- Non-technical family (test clarity)
- Marketing/business contacts (value prop feedback)
- Security-minded people (appreciate verification)

## 📧 Message Template

```
Hey! I just launched a demo of STAMP Protocol - a new approach to
fighting spam using formal verification (mathematical proofs).

Could you test it? Takes 2 minutes:

1. Open Telegram
2. Search: @stamp_demo_bot
3. Send: /start
4. Try: /verify, /status, /unsubscribe

Would love your feedback:
- Was it clear what's happening?
- Did the "proof" make sense?
- Would you trust this more than regular email?

Thanks! 🙏

Live site: https://stamp-protocol.org
```

## 📋 Feedback Form

Send testers this form to fill out:

```
STAMP Bot Feedback

Name: ___________
Date: ___________

1. First impressions (1-5): ☐☐☐☐☐

2. Did the consent proof make sense?
   ☐ Yes, totally clear
   ☐ Sort of understood it
   ☐ Confusing

3. Most valuable feature:
   ☐ Proven consent
   ☐ Working unsubscribe
   ☐ Rate limiting
   ☐ Transparency

4. Would you use this for:
   ☐ Email newsletters
   ☐ Social media
   ☐ Business messaging
   ☐ Other: ___________

5. What's confusing or broken?
   _________________________________
   _________________________________

6. What would make this better?
   _________________________________
   _________________________________

7. Would you recommend this? ☐ Yes ☐ Maybe ☐ No

8. Additional comments:
   _________________________________
   _________________________________
```

## 📊 Success Metrics

**Minimum viable feedback:**
- 3 people tested successfully
- At least 2 understood the value prop
- 0 critical bugs found
- Average rating: 3+/5

**Ideal feedback:**
- 5+ people tested
- All understood value prop
- Feature requests noted
- Average rating: 4+/5

## 🐛 Known Issues to Watch For

- Slow response times (bot lag)
- Unclear proof formatting
- Confusing terminology ("dependent types", "formal verification")
- Missing help text

## 📝 How to Collect Feedback

**Option 1: Direct messages**
- Send them the form above
- Ask follow-up questions
- Take notes

**Option 2: Video call**
- Watch them use it (screen share)
- Note where they get confused
- Ask questions as they go

**Option 3: Survey tool**
- Use Google Forms / Typeform
- Send link after they test
- Analyze responses

## 🔄 After Testing

1. **Compile feedback** → `~/stamp-bot-feedback.md`
2. **Identify patterns** (most common confusion)
3. **Prioritize fixes** (critical vs nice-to-have)
4. **Update bot** based on feedback
5. **Thank testers** (offer early access, credit them)

## 🎁 Tester Incentives

- Early access to production version
- Credit on website ("Thanks to our beta testers")
- Free tier when we monetize
- Swag (if/when available)

## ✅ Ready to Test

1. Make sure bot is running: `pgrep -f bot.ts`
2. Test it yourself first (all commands work)
3. Send message to 3-5 people
4. Collect feedback over next 2-3 days
5. Compile results

**Bot status check:**
```bash
cd ~/Documents/hyperpolymath-repos/stamp-telegram-bot
pgrep -f bot.ts && echo "✓ Bot running" || echo "✗ Start bot"
tail -20 bot.log  # Check for errors
```
