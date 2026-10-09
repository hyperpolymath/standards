<?php
/**
 * Basic PHP example with Palimpsest License integration
 */

require_once __DIR__ . '/../vendor/autoload.php';

use Palimpsest\PalimpsestConfig;
use Palimpsest\PalimpsestLicense;

// Configure Palimpsest License
$config = new PalimpsestConfig([
    'workTitle' => 'Voices of the Diaspora',
    'authorName' => 'Amara Okafor',
    'authorUrl' => 'https://example.com/amara',
    'emotionalLineage' => 'A collection of stories exploring migration, identity, and belonging',
    'version' => '0.4',
    'language' => 'en',
]);

$license = new PalimpsestLicense($config);

// Inject HTTP headers
$license->injectHeaders();

// Start output buffering
ob_start();
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Voices of the Diaspora</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
            max-width: 800px;
            margin: 40px auto;
            padding: 20px;
            line-height: 1.6;
        }
        .story {
            background: #f8f9fa;
            padding: 20px;
            margin: 20px 0;
            border-left: 4px solid #0366d6;
        }
    </style>
</head>
<body>
    <h1>Voices of the Diaspora</h1>

    <div class="story">
        <h2>The Journey</h2>
        <p>In 1985, my grandmother left Lagos with nothing but a suitcase and a dream...</p>
    </div>

    <?php echo $license->generateLicenseWidget('light'); ?>
</body>
</html>
<?php
// Get the buffered content and inject license metadata
$html = ob_get_clean();
echo $license->injectHtml($html);
