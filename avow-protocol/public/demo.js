let proofData = null;

async function loadProofData() {
  try {
    const res = await fetch("public/proof-data.json");
    if (!res.ok) {
      return null;
    }
    return await res.json();
  } catch (_err) {
    return null;
  }
}

function resultToString(result) {
  switch (result) {
    case "Success":
      return "✓ Verified";
    case "ErrorInvalidUrl":
      return "✗ Invalid URL";
    case "ErrorNotHttps":
      return "✗ Not HTTPS";
    case "ErrorInvalidConsent":
      return "✗ Invalid Consent";
    default:
      return "✗ Verification Failed";
  }
}

function resultToClass(result) {
  return result === "Success" ? "success" : "error";
}

function lookupProof(url) {
  if (!proofData || !proofData.urls) {
    return null;
  }
  return proofData.urls.find((entry) => entry.input === url) || null;
}

function lookupConsentProof() {
  if (!proofData || !proofData.consent || proofData.consent.length === 0) {
    return null;
  }
  return proofData.consent[0];
}

function verifyUnsubscribeLink(url) {
  const proof = lookupProof(url);
  if (proof) {
    if (!proof.parse_ok) {
      return { result: "ErrorInvalidUrl", source: "proven (build-time)" };
    }
    if (!proof.https) {
      return { result: "ErrorNotHttps", source: "proven (build-time)" };
    }
    return { result: "Success", source: "proven (build-time)" };
  }

  try {
    const parsed = new URL(url);
    if (parsed.protocol !== "https:") {
      return { result: "ErrorNotHttps", source: "runtime" };
    }
    return { result: "Success", source: "runtime" };
  } catch (_err) {
    return { result: "ErrorInvalidUrl", source: "runtime" };
  }
}

function verifyConsentChain(consent) {
  if (consent.confirmation <= consent.initialRequest || consent.token.length < 10) {
    return "ErrorInvalidConsent";
  }
  return "Success";
}

function testUnsubscribeLink(url) {
  const outputEl = document.getElementById("demo-output");
  outputEl.innerHTML = "<div class='demo-loading'>🔍 Validating link structure...</div>";
  setTimeout(() => {
    const now = Date.now();
    const verification = verifyUnsubscribeLink(url);
    const resultClass = resultToClass(verification.result);
    const resultText = resultToString(verification.result);
    const html = `
      <div class="demo-result ${resultClass}">
        <h4>${resultText}</h4>
        <div class="demo-details">
          <p><strong>URL:</strong> ${url}</p>
          <p><strong>Checked At:</strong> ${new Date(now).toISOString()}</p>
          <p><strong>Check:</strong> URL parses and is HTTPS</p>
          <p><strong>Source:</strong> ${verification.source}</p>
        </div>
      </div>
    `;
    outputEl.innerHTML = html;
  }, 300);
}

function testConsentChain() {
  const outputEl = document.getElementById("demo-output");
  outputEl.innerHTML = "<div class='demo-loading'>🔍 Verifying consent ordering...</div>";
  setTimeout(() => {
    const proof = lookupConsentProof();
    if (proof) {
      const result = proof.valid ? "Success" : "ErrorInvalidConsent";
      const resultClass = resultToClass(result);
      const resultText = resultToString(result);
      const html = `
        <div class="demo-result ${resultClass}">
          <h4>${resultText}</h4>
          <div class="demo-details">
            <p><strong>Initial Request:</strong> ${proof.initial_request}</p>
            <p><strong>Confirmation:</strong> ${proof.confirmation}</p>
            <p><strong>Token:</strong> ${proof.token}</p>
            <p><strong>Check:</strong> confirmation > initialRequest</p>
            <p><strong>Source:</strong> proven (build-time)</p>
          </div>
        </div>
      `;
      outputEl.innerHTML = html;
      return;
    }

    const now = Date.now();
    const initialRequest = now - 5000;
    const consent = {
      initialRequest,
      confirmation: now,
      token: "user_123_consent_token_abc",
    };
    const result = verifyConsentChain(consent);
    const resultClass = resultToClass(result);
    const resultText = resultToString(result);
    const html = `
      <div class="demo-result ${resultClass}">
        <h4>${resultText}</h4>
        <div class="demo-details">
          <p><strong>Initial Request:</strong> ${new Date(initialRequest).toISOString()}</p>
          <p><strong>Confirmation:</strong> ${new Date(now).toISOString()}</p>
          <p><strong>Token:</strong> ${consent.token}</p>
          <p><strong>Check:</strong> confirmation > initialRequest</p>
          <p><strong>Source:</strong> demo rules</p>
        </div>
      </div>
    `;
    outputEl.innerHTML = html;
  }, 300);
}

function initDemo() {
  const testUnsubBtn = document.getElementById("test-unsub-btn");
  const unsubUrlInput = document.getElementById("unsub-url");
  if (testUnsubBtn && unsubUrlInput) {
    testUnsubBtn.addEventListener("click", () => testUnsubscribeLink(unsubUrlInput.value));
  }
  const testConsentBtn = document.getElementById("test-consent-btn");
  if (testConsentBtn) {
    testConsentBtn.addEventListener("click", () => testConsentChain());
  }
}

document.addEventListener("DOMContentLoaded", async () => {
  proofData = await loadProofData();
  initDemo();
});
