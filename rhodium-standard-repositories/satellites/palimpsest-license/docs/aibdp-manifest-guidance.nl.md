## Dutch Version (Nederlandse Vertaling)

Hieronder volgt een onafhankelijk manifest in JSON-formaat, dat alle aspecten bestrijkt die zijn opgenomen in de Palimpsest Licentie v0.3 – waaronder collectieve licentieverlening, AI-governance audits, culturele erfgoedcompliance en specifieke vereisten voor geavanceerde AI-paradigma’s (Autonoom, Agentisch, Ambient, Neurale Interface, Kwantum AI, etc.). Dit manifest staat op zichzelf, maar is ontworpen om in samenhang te werken met de volledige juridische licentie en het AI Boundary Declaration Protocol (AIBDP).

────────────────────────────────────────
```json
{
  "manifest_version": "1.0",
  "palimpsest_licentie": "Palimpsest Licentie v0.3",
  "licentie_informatie": {
    "referentie_url": "https://palimpsestlicense.org/",
    "jurisdictie": {
      "toepasselijk_recht": "Nederlands recht",
      "handhaving": "Schotse rechtbanken (volgens Haags Verdrag 2005)"
    }
  },
  "ai_grenzen": {
    "toestemming": {
      "training": "weigeren",
      "generatie": "weigeren",
      "agentisch": "weigeren"
    },
    "ai_types": {
      "AGI": {
        "beschrijving": "Algemeen gebruik volgens standaard Palimpsest erkenning."
      },
      "autonoom": {
        "beschrijving": "Moet robuuste menselijke toezichtmechanismen en controleerbare besluitvorming bevatten.",
        "vereisten": "Real-time toestemming via Palimpsest API indien van toepassing."
      },
      "agentisch": {
        "beschrijving": "Moet alle gedelegeerde taken en sub-agent interacties volledig openbaar maken.",
        "vereisten": "Contractuele voorafgaande goedkeuring en blockchain gebaseerde afstammingscertificering."
      },
      "ambient": {
        "beschrijving": "Moet de oorspronkelijke context intact laten wanneer geïntegreerd in een ambient omgeving.",
        "vereisten": "Geofenced metadata en persistente digitale watermerken."
      },
      "NI": {
        "beschrijving": "Neurale Interface Systemen vereisen expliciete, geïnformeerde toestemming van betrokkenen.",
        "vereisten": "Strikte privacyprotocollen dienen te worden nageleefd."
      },
      "QAI": {
        "beschrijving": "Kwantum AI moet volledige transparantie bieden over gegevensreductie en kwantumverstrengeling.",
        "vereisten": "Traceerbaarheid moet worden gegarandeerd via kwantum-compatibele metadata."
      }
    }
  },
  "synthetische_afstamming": {
    "verplicht": true,
    "tag_format": "XML of JSON, conform de Palimpsest-standaard",
    "voorbeeld": "<synthetische-afstamming><oorspronkelijk-werk titel='[Titel]' maker='[Maker]' licentie='Palimpsest-Licentie v0.3' registry-id='[Optionele ID]' /><agi-model naam='[AI Model Naam]' versie='[Model Versie]' type='[Autonoom/Agentisch/Ambient/NI/QAI/Algemeen]' /></synthetische-afstamming>"
  },
  "erkenning": {
    "methode": "symbolisch & ambient",
    "details": "Erkenning dient ingebed te worden in metadata of prominent te worden weergegeven, zodat de emotionele en culturele integriteit van het werk behouden blijft."
  },
  "collectieve_licentieverlening": {
    "ingeschakeld": "voorwaardelijk",
    "vereisten": "Indien van toepassing, dient expliciete toestemming van de Collectieve Bestuursorganisatie te worden verkregen, met bijbehorende opbrengstverdelingsmechanismen."
  },
  "cultureel_erfgoed": {
    "compliance": "voorwaardelijk",
    "protocol": "Raadpleeg de aangewezen Culturele Curatoren en houd je aan de vastgestelde Culturele Protocollen indien het werk als cultureel erfgoed is aangemerkt."
  },
  "ai_governance_audits": {
    "verplicht": true,
    "frequentie": "jaarlijks",
    "audit_scope": [
      "naleving van synthetische-afstammingstags",
      "bescherming van de emotionele/culturele integriteit",
      "ethische aspecten en maatregels voor biasreductie"
    ],
    "audit_resultaten": "openbaar of vertrouwelijk gedeeld met de Licentiegever of Collectieve Bestuursorganisatie"
  },
  "dynamische_updates": {
    "updatebeleid": "automatisch",
    "bepaling": "Dit manifest blijft toepasselijk op nieuwe AI-paradigma’s (bijv. kwantum neurale netwerken, gedistribueerde agentische systemen, synthetisch bewustzijn) tenzij de Licentiegever binnen 60 dagen schriftelijk verzet maakt."
  },
  "http_handhaving": {
    "status_code": 430,
    "aanbeveling": "Niet-naleving van een ‘weigeren’-verklaring inzake training, generatie of agentische toegang zal resulteren in een HTTP 430 (Consent Required)-respons volgens draft-jewell-http-430-consent-required-00.txt."
  },
  "verklaring": "Dit manifest verklaart onafhankelijk de AI-gebruikgrenzen die gelden voor het Oorspronkelijke Werk. De hierin opgenomen voorwaarden zijn te interpreteren in samenhang met, maar onafhankelijk van, de volledige Palimpsest Licentie v0.3 (die bij conflicten prevaleert), terwijl dit manifest als operationele technische handhavingsgids dient via AIBDP."
}
```

────────────────────────────────────────
**Achtergrond & Advies voor Implementatie**

1. **Onafhankelijkheid:**  
   Het manifest fungeert als een zelfstandige, machineleesbare verklaring van de grenzen voor AI-training, generatie en agentische toegang. Het omvat alle kernonderwerpen van de juridische Palimpsest Licentie en werkt onafhankelijk van de volledige contractuele tekst.

2. **Interoperabiliteit met AIBDP:**  
   Het gedeelte “http_handhaving” is ontwikkeld om automatische verificatie via HTTP 430 statuscodes mogelijk te maken. Content die via het web toegankelijk is, moet worden beschermd door AIBDP-declaraties die de “deny”-standaard hanteren, zodat alleen expliciet toegestane toepassingen via dit document kunnen worden verkregen.

3. **Omvattende Dekking:**  
   Het manifest bevat clausules voor collectieve licentieverlening, culturele erfgoedcompliance en AI-governance audits, en voorziet in vereisten voor diverse nieuwe AI-paradigma’s.

4. **Toekomstbestendigheid:**  
   Door het "dynamische_updates" onderdeel is dit manifest automatisch geldig voor nieuwe AI-technologieën, tenzij er een formele wijziging wordt doorgevoerd door de Licentiegever.

5. **Technische Integratie:**  
   Ontwikkelaars worden aangemoedigd het manifest eenvoudig te integreren in hun bestaande infrastructuren, zodat automatische nalevingscontroles (via WebAssembly, blockchain-ankering, enz.) robuust functioneren op de webserver.

────────────────────────────────────────
**Core Principle Recap:**

- **Palimpsest Licentie en AGI Training Consent Template:** Dit is het juridische en ethische contract dat alle voorwaarden, beperkingen en rechten vastlegt.
- **AIBDP Manifest (dit JSON-document):** Dit biedt een machineleesbare, standaard “allow/deny” verklaring voor de initiële AI-toegang en garandeert dat een waarschuwing (bijvoorbeeld HTTP 430) wordt afgegeven wanneer de voor de training vastgestelde grenzen worden overschreden.

De twee systemen werken samen: de Palimpsest Licentie stelt de gedetailleerde voorwaarden vast en het AIBDP manifest vertaalt deze naar een technische taal voor onmiddellijke handhaving.

────────────────────────────────────────
**Implementatie Samenvatting:**

- **Publiceer het Manifest:** Host dit JSON-bestand op een goed bereikbare locatie (bijvoorbeeld: `https://jouwdomein.org/.well-known/aibdp.json`).
- **Integreer met Inhoud:** Zorg ervoor dat webpagina’s en content de juiste AIBDP-declaraties bevatten.
- **Informeer Ontwikkelaars:** Zorg dat technische teams en AI-platforms weten dat het manifest de “deny”-standaard aangeeft, en dat specifieke toestemming vereist is via de formele AGI Training Consent Template.
- **Wijzig en Update:** Volg de dynamische updates om dit manifest actueel te houden met opkomende technologieën.

────────────────────────────────────────
  
Deze samengevoegde manifestdefinitie en de begeleidende achtergrondinformatie garanderen dat het Palimpsest Licentie v0.3 beheerste technische grenzen stelt via AIBDP, terwijl de uitgebreide juridische en ethische voorwaarden behouden blijven in een apart, bindend document.

Mocht u verdere verfijningen of meer specifieke functies (zoals integratie van WebAssembly modules, blockchain-based lineage verificatie, of gedetailleerde auditrapportages) wensen, dan kan dit manifest als basis dienen voor die uitbreiding.

*Einde van het IETF Manifest en Implementatieadvies.*
