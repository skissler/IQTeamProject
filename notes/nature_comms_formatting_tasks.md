# Nature Communications Formatting Task List

Targeted tasks for preparing the manuscript (`writeup/agri_resp_full_2026-03-17.docx`) for initial submission to Nature Communications.

## Critical (Submission will fail or be desk-rejected without these)

### 1. Restructure the abstract
- **Current**: Structured with Background/Methods/Results/Conclusions headers; ~210 words; no references (good).
- **Required**: Unstructured (remove headers), max 200 words. Trim ~10 words and merge into a single flowing paragraph.

### 2. Reorder main text sections
- **Current order**: Introduction → Methods → Results → Discussion
- **Required order**: Introduction → **Results** → **Discussion** → **Methods**
- Nature Comms specifies: *"begin with Introduction, followed by Results, Discussion, and Methods."*

### 3. Shorten the title
- **Current**: *"Modeling the impact of respiratory illness outbreaks on the agricultural workforce and food production in the United States"* (18 words)
- **Required**: Max 15 words. Suggestion: *"Respiratory disease outbreaks disproportionately affect agricultural workers and food production"* (10 words) or similar.

### 4. Fill in empty required sections
- **Author contributions**: Currently empty. Nature Comms requires this. Each author's contribution must be stated (e.g., "K.B., L.X.P., and E.K.C. conceived the study...").
- **Competing interests**: Currently empty. Must have an explicit statement even if "The authors declare no competing interests."

### 5. Separate Supplementary Information into its own file
- Currently the main .docx includes everything from "Supplementary Information" (line 269 onward) — supplementary methods, 14 supplementary figures, and 6 supplementary tables — all embedded in the main manuscript file.
- Nature Comms requires: *"Supplementary Information should be combined and supplied as a separate file, preferably in Word format."*
- Create a standalone `Supplementary_Information.docx` containing everything from "Supplementary Methods" through Table S6.

---

## Important (Strongly recommended for initial submission)

### 6. Check main text word count against the ~5,000 word target
- The main text (Introduction + Results + Discussion, excluding Abstract, Methods, References, Figure legends) looks to be ~3,500-4,000 words — likely fine, but verify after reordering.
- The Methods section should ideally be <3,000 words. Currently ~2,500 words; likely fine.

### 7. Reformat figure legends
- **Current**: Figure legends appear inline after the Discussion, interleaved with figure placeholders.
- **Required**: *"Figure Legends"* should appear as a section after References, with all legends listed in numerical order. Remove the `[placeholder]` image slots — for initial submission, figures can either be embedded at appropriate positions OR grouped at the end, but legends must be in the text file.

### 8. Move tables to end of document
- The supplementary tables are already in the SI, but verify there are no main-text tables. Currently, the 3 main figures appear to be the only display items in the main text. The main-text crop impact results are reported inline, which is fine.

### 9. Separate the Funding section
- Currently funding is under "Acknowledgments" → "Funding" as a subsection.
- Nature Comms wants a distinct **"Funding"** section separate from Acknowledgments. The Acknowledgments section itself should not contain funding info.

### 10. Expand the Data Availability statement
- **Current**: *"All data and code associated with this manuscript can be accessed at https://github.com/skissler/IQTeamProject"*
- **Should specify**: which datasets are public vs. restricted (ACS is public, NAWS requires application), what's in the repository, and ideally provide a DOI via Zenodo or similar for a frozen release.

### 11. Add a Code Availability statement
- Nature Comms distinguishes Data Availability from Code Availability. Consider splitting into two statements. Nature Comms also offers Code Ocean integration — consider setting up a capsule, though not strictly required for initial submission.

### 12. Prepare a cover letter
- Required at submission. Should include:
  - Corresponding author contact info
  - Brief summary of why the work is appropriate for Nature Comms' diverse readership
  - Suggested reviewers (names + contact info) and any exclusions
  - Statement about prior discussions with editors (if any)

---

## Minor / Recommended

### 13. Reference formatting
- 65 references — under the 70 limit, good.
- Verify format matches Nature style: *Author, A. B. & Author, C. D. Title of article. Journal **Vol**, pages (year).*
- Several references appear to be web pages or reports (refs 7, 26, 35, 36-38, 41-44, 47) — verify these are formatted correctly.
- Reference 21 appears incomplete (has a DOI but no journal/year).

### 14. Display item count
- 3 main figures — well under the 10-item limit. Consider whether any key supplementary figures (e.g., Figure S4 sensitivity overview, or the new Table S6 R0 comparison) should be promoted to the main text to strengthen the narrative. Nature Comms allows up to 10.

### 15. Formatting details
- Double-spaced, single-column, numbered pages (verify in Word).
- Use standard fonts (e.g., Times New Roman, Arial).
- **Corresponding author**: Currently `*` denotes equal contribution and `†` also denotes equal contribution. Need to designate one corresponding author with `*` per Nature Comms convention, and use a different convention for equal contribution.
- **ORCID**: Corresponding authors should link ORCIDs in the submission system (not a manuscript change, but a submission-time task).

### 16. Ensure SI is self-contained
- The supplementary methods reference equations and parameters from the main text. SI should ideally be self-contained per Nature Comms guidelines: *"Supplementary Information is self-contained (it should not refer to the list of References in the main paper)"*. Currently SI references are numbered sequentially continuing from the main text — these need to be renumbered starting from 1 if they're in a separate file, or duplicated.

### 17. LLM disclosure
- If Claude Code or other LLMs were used in analysis or writing, Nature Comms requires disclosure in the Methods section.

---

## Effort summary

| Priority | Tasks | Est. effort |
|----------|-------|-------------|
| Critical | 1-5 | ~2-3 hours |
| Important | 6-12 | ~2-4 hours |
| Minor | 13-17 | ~1-2 hours |

The critical items (1-5) are the minimum to submit. Items 6-12 will strengthen the submission and avoid a desk return for reformatting. Items 13-17 are polish.
