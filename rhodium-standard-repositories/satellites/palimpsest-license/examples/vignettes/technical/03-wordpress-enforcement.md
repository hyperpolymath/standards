# Technical Vignette: WordPress Blog with License Enforcement

## Scenario: Content Management System Integration of Palimpsest

**Site**: *DiasporaVoices.org* (community blog platform)
**Platform**: WordPress with custom Palimpsest plugin
**Challenge**: Enforcing license terms in CMS, protecting contributor content
**Outcome**: Automated compliance, community protection, successful platform growth

---

## The Challenge

DiasporaVoices.org hosts personal essays from diaspora writers—refugees, immigrants, first-generation citizens sharing stories of displacement, belonging, and cultural navigation.

Writers wanted:
- Free public access to their stories (no paywalls)
- Protection from commercial exploitation (no content farms scraping their trauma narratives)
- Cultural context preservation (essays aren't generic "immigrant stories")
- AI training consent (no chatbots trained on their personal experiences)
- Easy publishing (WordPress-familiar workflow)

**Technical requirements:**
1. Apply Palimpsest license automatically to posts
2. Display license terms prominently
3. Embed metadata in posts (preserve attribution, cultural context)
4. Prevent scraping/copying where possible
5. Make permission requests easy
6. Track usage and violations

### The Solution: Custom WordPress Plugin

**Plugin name**: `Palimpsest License Manager`

**Features:**
- Automatic license application to posts
- Custom post metadata for cultural context
- License badge display
- Right-click protection (with accessibility exceptions)
- RSS feed attribution preservation
- Contact form for permission requests
- Analytics for tracking potential violations

---

## Technical Implementation

**1. Plugin Structure:**
```php
<?php
/**
 * Plugin Name: Palimpsest License Manager
 * Description: Manage Palimpsest v0.4 licensing for WordPress content
 * Version: 1.2.0
 * Author: DiasporaVoices Collective
 * License: GPL-3.0
 */

// Add Palimpsest metadata fields to post editor
add_action('add_meta_boxes', 'palimpsest_add_meta_boxes');
function palimpsest_add_meta_boxes() {
    add_meta_box(
        'palimpsest_metadata',
        'Palimpsest License Metadata',
        'palimpsest_metadata_callback',
        'post',
        'normal',
        'high'
    );
}

function palimpsest_metadata_callback($post) {
    wp_nonce_field('palimpsest_save_metadata', 'palimpsest_nonce');

    $cultural_context = get_post_meta($post->ID, '_palimpsest_cultural_context', true);
    $emotional_intent = get_post_meta($post->ID, '_palimpsest_emotional_intent', true);
    $content_warnings = get_post_meta($post->ID, '_palimpsest_content_warnings', true);
    $ai_training_consent = get_post_meta($post->ID, '_palimpsest_ai_consent', true);

    ?>
    <p>
        <label for="palimpsest_cultural_context">Cultural Context:</label><br>
        <textarea id="palimpsest_cultural_context" name="palimpsest_cultural_context" rows="3" style="width:100%"><?php echo esc_textarea($cultural_context); ?></textarea>
        <span class="description">Describe the cultural background, diaspora community, or heritage this piece relates to.</span>
    </p>

    <p>
        <label for="palimpsest_emotional_intent">Emotional Intent:</label><br>
        <textarea id="palimpsest_emotional_intent" name="palimpsest_emotional_intent" rows="2" style="width:100%"><?php echo esc_textarea($emotional_intent); ?></textarea>
        <span class="description">What emotional experience or understanding do you hope readers take away?</span>
    </p>

    <p>
        <label for="palimpsest_content_warnings">Content Warnings (if applicable):</label><br>
        <input type="text" id="palimpsest_content_warnings" name="palimpsest_content_warnings" value="<?php echo esc_attr($content_warnings); ?>" style="width:100%">
        <span class="description">E.g., "trauma, displacement, violence"</span>
    </p>

    <p>
        <label>
            <input type="checkbox" name="palimpsest_ai_consent" value="1" <?php checked($ai_training_consent, '1'); ?>>
            I consent to this piece being used for interpretive AI training
        </label><br>
        <span class="description">⚠️ By default, AI training is NOT permitted. Check this only if you explicitly allow it.</span>
    </p>
    <?php
}

// Save metadata
add_action('save_post', 'palimpsest_save_metadata');
function palimpsest_save_metadata($post_id) {
    if (!isset($_POST['palimpsest_nonce']) || !wp_verify_nonce($_POST['palimpsest_nonce'], 'palimpsest_save_metadata')) {
        return;
    }

    if (defined('DOING_AUTOSAVE') && DOING_AUTOSAVE) {
        return;
    }

    if (isset($_POST['palimpsest_cultural_context'])) {
        update_post_meta($post_id, '_palimpsest_cultural_context', sanitize_textarea_field($_POST['palimpsest_cultural_context']));
    }

    if (isset($_POST['palimpsest_emotional_intent'])) {
        update_post_meta($post_id, '_palimpsest_emotional_intent', sanitize_textarea_field($_POST['palimpsest_emotional_intent']));
    }

    if (isset($_POST['palimpsest_content_warnings'])) {
        update_post_meta($post_id, '_palimpsest_content_warnings', sanitize_text_field($_POST['palimpsest_content_warnings']));
    }

    $ai_consent = isset($_POST['palimpsest_ai_consent']) ? '1' : '0';
    update_post_meta($post_id, '_palimpsest_ai_consent', $ai_consent);

    // Automatically apply Palimpsest license
    update_post_meta($post_id, '_palimpsest_license_version', '0.4');
    update_post_meta($post_id, '_palimpsest_license_applied', current_time('mysql'));
}

// Display license badge and metadata on posts
add_filter('the_content', 'palimpsest_append_license_info');
function palimpsest_append_license_info($content) {
    if (!is_single()) {
        return $content;
    }

    global $post;

    $cultural_context = get_post_meta($post->ID, '_palimpsest_cultural_context', true);
    $emotional_intent = get_post_meta($post->ID, '_palimpsest_emotional_intent', true);
    $content_warnings = get_post_meta($post->ID, '_palimpsest_content_warnings', true);
    $ai_consent = get_post_meta($post->ID, '_palimpsest_ai_consent', true);

    $license_html = '<div class="palimpsest-license-info" style="border:2px solid #3498db; padding:20px; margin:30px 0; background:#f0f8ff;">';

    // Content warnings (if any)
    if (!empty($content_warnings)) {
        $license_html .= '<p style="color:#e74c3c; font-weight:bold;">⚠️ Content Warnings: ' . esc_html($content_warnings) . '</p>';
    }

    // License badge
    $license_html .= '<p><strong>📜 License:</strong> <a href="https://palimpsest.license/v0.4" target="_blank">Palimpsest v0.4</a></p>';

    // Author attribution
    $license_html .= '<p><strong>✍️ Author:</strong> ' . get_the_author() . '</p>';

    // Cultural context
    if (!empty($cultural_context)) {
        $license_html .= '<p><strong>🌍 Cultural Context:</strong> ' . esc_html($cultural_context) . '</p>';
    }

    // Emotional intent
    if (!empty($emotional_intent)) {
        $license_html .= '<p><strong>💭 Emotional Intent:</strong> ' . esc_html($emotional_intent) . '</p>';
    }

    // AI training status
    $ai_status = ($ai_consent == '1') ? '✅ AI training permitted with attribution' : '🚫 AI training NOT permitted without explicit consent';
    $license_html .= '<p><strong>🤖 AI Training:</strong> ' . $ai_status . '</p>';

    // Usage guidelines
    $license_html .= '
    <details>
        <summary style="cursor:pointer; font-weight:bold;">📖 How You Can Use This Piece</summary>
        <ul style="margin-top:10px;">
            <li>✅ Share on social media with attribution</li>
            <li>✅ Quote in educational or journalistic contexts</li>
            <li>✅ Translate to other languages (preserving context)</li>
            <li>✅ Use in community organizing or advocacy</li>
            <li>🚫 Commercial republication without permission</li>
            <li>🚫 Stripping cultural context or author attribution</li>
            <li>🚫 AI training (unless explicitly permitted above)</li>
        </ul>
        <p><a href="' . home_url('/request-permission') . '">Request Special Permission</a></p>
    </details>';

    $license_html .= '</div>';

    return $content . $license_html;
}

// Add machine-readable metadata to post head
add_action('wp_head', 'palimpsest_add_json_ld');
function palimpsest_add_json_ld() {
    if (!is_single()) {
        return;
    }

    global $post;

    $cultural_context = get_post_meta($post->ID, '_palimpsest_cultural_context', true);
    $ai_consent = get_post_meta($post->ID, '_palimpsest_ai_consent', true);

    $json_ld = [
        '@context' => 'https://schema.org',
        '@type' => 'Article',
        'headline' => get_the_title(),
        'author' => [
            '@type' => 'Person',
            'name' => get_the_author()
        ],
        'datePublished' => get_the_date('c'),
        'license' => 'https://palimpsest.license/v0.4',
        'culturalContext' => $cultural_context,
        'aiTrainingConsent' => ($ai_consent == '1') ? 'permitted' : 'prohibited'
    ];

    echo '<script type="application/ld+json">' . json_encode($json_ld, JSON_UNESCAPED_SLASHES | JSON_PRETTY_PRINT) . '</script>';
}

// Modify RSS feed to include attribution
add_filter('the_excerpt_rss', 'palimpsest_rss_attribution');
add_filter('the_content_feed', 'palimpsest_rss_attribution');
function palimpsest_rss_attribution($content) {
    global $post;
    $attribution = '<p><em>This piece is licensed under Palimpsest v0.4. Full cultural context and usage terms: ' . get_permalink($post->ID) . '</em></p>';
    return $content . $attribution;
}

// Add permission request page template
add_action('init', 'palimpsest_create_permission_page');
function palimpsest_create_permission_page() {
    // Creates /request-permission page with contact form
    // (Implementation would use Contact Form 7 or similar)
}
?>
```

**2. Frontend Display (Example Post):**

```
Title: "Between Two Worlds: Ramadan in Edinburgh"

[Post content...]

┌────────────────────────────────────────────────────────────┐
│ 📜 License: Palimpsest v0.4                                 │
│                                                             │
│ ✍️ Author: Amira Hassan                                     │
│                                                             │
│ 🌍 Cultural Context: Written from the perspective of a      │
│ British-Somali Muslim navigating religious practice in a   │
│ Scottish context. Explores the tension between maintaining │
│ cultural traditions and adapting to a new environment.      │
│                                                             │
│ 💭 Emotional Intent: I hope readers—especially other        │
│ diaspora Muslims—feel less alone in navigating these dual  │
│ identities. For non-Muslim readers, I hope this builds     │
│ empathy for the complexity of diaspora life.                │
│                                                             │
│ 🤖 AI Training: 🚫 NOT permitted without explicit consent   │
│                                                             │
│ 📖 How You Can Use This Piece [expand ▼]                   │
└────────────────────────────────────────────────────────────┘
```

**3. Anti-Scraping Measures:**

```javascript
// assets/palimpsest-protection.js
(function() {
    'use strict';

    // Disable right-click on post content (with accessibility note)
    document.querySelector('.entry-content').addEventListener('contextmenu', function(e) {
        // Allow for accessibility tools (screen readers, etc.)
        if (e.target.getAttribute('data-accessibility-exception')) {
            return true;
        }

        e.preventDefault();
        alert('This content is protected under Palimpsest License v0.4. Please request permission for reuse: [contact link]');
        return false;
    });

    // Watermark copied text with attribution
    document.querySelector('.entry-content').addEventListener('copy', function(e) {
        const selection = window.getSelection().toString();
        const attribution = '\n\n[From: ' + document.title + ' | License: Palimpsest v0.4 | ' + window.location.href + ']';

        e.clipboardData.setData('text/plain', selection + attribution);
        e.preventDefault();
    });

    // Detect potential scraping (rapid content access)
    let pageViews = 0;
    const maxViewsPerMinute = 10;

    setInterval(function() {
        if (pageViews > maxViewsPerMinute) {
            // Log potential scraping attempt
            fetch('/wp-json/palimpsest/v1/log-suspicious-activity', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({
                    ip: 'client-ip',
                    timestamp: Date.now(),
                    reason: 'excessive-page-views'
                })
            });
        }
        pageViews = 0;
    }, 60000);

    window.addEventListener('load', function() {
        pageViews++;
    });
})();
```

---

## Results

**Year 1:**
- 200 writers published 800+ essays
- 500,000 page views
- Zero unauthorized commercial republications reported
- 15 permission requests (12 approved)
- 98% of writers reported feeling "protected and respected" by the platform

**Violation Prevention:**
- Right-click protection deterred casual copying
- Attribution in copied text educated users
- Scraping detection caught 5 attempted content farms (blocked)
- Prominent license display reduced "didn't know" excuses

**Community Feedback:**
- "Finally, a platform that understands our stories have value beyond clicks."
- "The cultural context fields helped me articulate why my piece matters."
- "Knowing AI can't train on my trauma narrative without asking gives me peace."

---

## Best Practices

### For WordPress Site Admins

**Do:**
- Implement Palimpsest metadata fields in post editor
- Display license information prominently
- Make permission requests easy (contact forms)
- Educate writers about their rights
- Monitor for scraping attempts

**Don't:**
- Apply Palimpsest without author consent
- Hide license information in footers
- Ignore permission requests
- Disable all copy functionality (accessibility matters)

### For Content Creators

**Do:**
- Fill out cultural context fields thoughtfully
- Use content warnings appropriately
- Decide on AI training consent deliberately
- Link to license terms when sharing on social media
- Report violations to site admins

**Don't:**
- Leave metadata fields blank
- Assume the license protects without visibility
- Over-use content warnings (dilutes their impact)

---

## Discussion Questions

1. Should WordPress.org include Palimpsest as a built-in license option? Or is it too niche?

2. Are technical protection measures (right-click blocking) helpful or do they harm accessibility more than they help protection?

3. Should writers be able to apply different licenses to different posts on the same site? Or should sites enforce one standard?

4. How can platforms balance "free public access" with "protection from exploitation"? Is that tension resolvable?

5. Should diaspora writers retain copyright or transfer it to community archives? What's the best stewardship model?

---

**License Note**: This vignette is licensed under CC BY-SA 4.0. DiasporaVoices.org and the Palimpsest License Manager plugin are fictional, created to illustrate WordPress integration of Palimpsest. The technical patterns are based on real WordPress development practices.
