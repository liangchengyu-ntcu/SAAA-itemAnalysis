# SAAA-itemAnalysis (縣市學生學習能力檢測｜試題與成績分析系統)

這是一個本機與雲端通用 (Posit Connect 相容) 的 R Shiny 專業分析系統，專為縣市學能檢測、學校測驗及學術研究設計。提供前置資料清洗、多檔案合併與拆分、批次試卷計分、雙向度 Scoring 引擎、經典測驗理論 (CTT) 試題分析、縣市標準三等級誘答力診斷及美化 Excel 報表匯出。

---

## 🌟 核心特色

- **獨立資料清洗與檔案處理**：提供欄名標準化、性別補齊與修復、特教整併、學校名錄校對，以及總流水號插入 Column A 之檔案合併與拆分。
- **靈活分析模式**：
  - **單科模式**：1 份答案檔搭配一個或多個年級作答檔。
  - **批次模式**：上傳含多科、多年級資料的 ZIP。
  - **獨立 CTT 試題分析頁籤**：可直接在「試題分析 (CTT)」頁籤上傳檔案並執行分析。
- **雙重 CTT 試題分析演算法引擎**：
  - **傳統 27% 高低分組模式**（Kelley 27% 臨界法）：包含難度、鑑別度、pBis、Cronbach's $\alpha$ 信度及智慧試題警告。
  - **縣市標準三等級模式**（對齊國立臺中教育大學測驗統計中心規範）：依據精熟/基礎門檻，計算全體、精熟、基礎、待加強選答率矩陣。
- **英語多維度 / 聽讀劃分 Scoring 引擎**：自動解析答案檔多向度欄位（如 `E41_向度` 聽力/閱讀），產出各向度分數與分項指標。
- **全自動高併發與錯誤隔離**：背景程序計算、預覽與 Excel 共用資料來源，單一工作失敗不影響整體批次。

---

## 🚀 快速開始

### 環境需求
- R 4.3 以上
- 建議使用 RStudio 開啟 `SAAA-itemAnalysis.Rproj`。

```r
# 安裝與更新必要套件
source("install_dependencies.R")

# 啟動應用程式
shiny::runApp()
```

命令列啟動方式：

```bash
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
```

*註：預設單次上傳上限為 500 MB。可在啟動前設定環境變數 `SCORE_APP_MAX_UPLOAD_MB` 調整。*

---

## 📖 導覽頁籤與使用流程

1. **資料清洗**：針對格式不一的原始資料檔，執行欄位對照、性別補齊、特教註記整合與名錄校對。
2. **合併與拆分**：自動插入 `總流水號` 至 Column A（1st column），並重新整理檔案結構。
3. **成績計算與報表**：
   - 選擇「單科」或「整批 ZIP」，上傳答案檔與作答檔。
   - 按「檢查檔案」確認格式無誤後按「開始計算」。
   - 可在頁面預覽各類成績卡、分數分布圖與 11 份成績 Excel。
4. **試題分析 (CTT)**：
   - 提供獨立的試題品質診斷介面與專屬檔案上傳卡。
   - 可自由切換 **「傳統 27% 高低分組」** 或 **「標準三等級 (精熟/基礎/待加強)」** 雙模式。
   - 呈現 Cronbach's $\alpha$ 信度卡、試題診斷總表、誘答力矩陣表與專屬 Excel 下載。
5. **使用說明**：包含詳細操作範例與匿名測試檔案下載。

---

## 🧮 核心演算法與統計規則說明

### 1. 學生身分與統計母體劃分
- **統計摘要互斥關係**：`學生數 = 到考數 + 缺考數 + 特殊生`。
  - **缺考**：整份有效題目均未作答者（答案為 `"9"` 或全部空白）優先歸為缺考。
  - **特殊生**：特殊生欄位標記為 `1`、`2`、`3` 者歸為特殊生。
  - **到考**：其餘學生歸為有效到考母體。
- **平均數計算**：全體及分組平均一律**排除缺考者與特殊生**。
- **排名與 PR 值**：排名與 PR 值的母體為**所有非缺考學生**。

---

### 2. 傳統 27% 高低分組 CTT 試題分析演算法 ([R/35_ctt.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/35_ctt.R))

針對有效到考受試者之總分進行排序，採用 **Kelley (1939) 27% 臨界分組演算法**：

```r
# 27% 高低分組演算法（嚴格保留不變）
x <- round(n_total_valid * 0.27, 0)
sorted_scores <- sort(scores_valid)
threshold_low <- sorted_scores[x]
threshold_high <- sorted_scores[n_total_valid - x + 1]

group <- rep("mid68", n_total_valid)
group[scores_valid <= threshold_low] <- "lower"
group[scores_valid >= threshold_high] <- "upper"
```

- **通過率 / 全體答對率 ($P_{rate}$)**：
  $$P_{rate} = \frac{\text{全體答對人數 } R}{\text{全體有效人數 } N}$$
- **CTT 難度 ($P_{diff}$)**：基於極端高低分組答對率之平均值：
  $$P_{diff} = \frac{P_{upper} + P_{lower}}{2}$$
- **鑑別度 ($D$)**：
  $$D = P_{upper} - P_{lower}$$
- **點二系列相關係數 ($r_{pbis}$)**：
  $$r_{pbis} = \text{Pearson correlation between item score (0/1) and total score}$$
- **Cronbach's $\alpha$ 信度**：
  $$\alpha = \frac{K}{K-1} \left(1 - \frac{\sum_{i=1}^K \sigma_i^2}{\sigma_T^2}\right)$$
- **智慧試題警告與填色機制**：
  - 🔴 **紅底/紅字 (鑑別度 $< 0.05$)**：標註「鑑別度未達 0.05 建議直接刪除」。
  - 🟢 **綠底/綠字 (鑑別度 $0.05 \sim 0.15$)**：標註「鑑別度 0.05～0.15 需進行試題修改」。
  - 🟡 **黃底 (高分組誘答異常)**：高分組選擇某錯誤選項之比率高於正確選項。
  - 🩶 **灰底 (全體誘答異常)**：全體選擇某錯誤選項之比率高於正確選項。
  - 答案空白之題目自動判定為「該題不予計分」並進行跨欄合併。

---

### 3. 縣市標準三等級（精熟 / 基礎 / 待加強）試題分析演算法 ([R/35_ctt.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/35_ctt.R))

**對齊國立臺中教育大學測驗統計與適性學習研究中心（縣市學檢標準規範）**：

- **標準門檻切分**：
  - **精熟 (Mastery)**：答對題數 $\ge$ `mastery_cutoff`
  - **基礎 (Basic)**：`basic_cutoff` $\le$ 答對題數 $<$ `mastery_cutoff`
  - **待加強 (Needs Improvement)**：答對題數 $<$ `basic_cutoff`
- **四群體選答率矩陣**：
  逐題計算 **全體 (Overall)**、**精熟 (Mastery)**、**基礎 (Basic)**、**待加強 (Needs Improvement)** 四組學生選擇選項 `1`, `2`, `3`, `4` (或 `A`, `B`, `C`, `D`) 與 `其它` 的百分比：
  $$\text{等級選答率} = \frac{\text{該等級選擇選項 } k \text{ 之人數}}{\text{該等級總人數}}$$
  *特色：可精準分析「待加強」學生集中選了哪個錯誤選項，找出關鍵迷思概念！*

---

### 4. 英語與多維度得分解析演算法 ([R/10_validation.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/10_validation.R))

使用 `resolve_dimension_columns()` 正則表達式自動比對答案檔欄位：
- Pattern 1: `^<SUBJ><GRD>[0-9]+_向度$` (如 `E41_向度` 聽力/閱讀, `E42_向度` 子能力向度)
- Pattern 2: `^<SUBJ><GRD>_[0-9]+_向度$` (如 `E4_1_向度`)
- Pattern 3: `^<SUBJ><GRD>_?向度$` (如 `E4向度`, `S4向度`)

自動計算主向度（如聽力、閱讀）與子向度分項得分，並寫入所有個人與彙總報表中。

---

## 📂 輸出報表與檔案清單

每個科目／年級分析完成後，系統會生成 **15 份 Excel 報表** 並壓縮打包提供下載。所有 6 大彙總表均已全面擴充 **標準三等級（精熟 / 基礎 / 待加強）人數與百分比**：

| 報表類型 | 檔名後綴 / 格式 | 說明與欄位架構 |
| :--- | :--- | :--- |
| **1. 全體總答對率** | `全體總答對率.xlsx` | 學生總分與答對率明細 |
| **2. 縣市平均** | `_縣市平均.xlsx` | `縣市` \| `總答對率` \| `向度明細` |
| **3. 各校平均** | `_各校平均.xlsx` | `學校代碼` \| `學校名稱` \| `總答對率` \| `向度明細` |
| **4. 各班平均** | `_各班平均.xlsx` | `學校代碼` \| `班級代碼` \| `總答對率` \| `向度明細` |
| **5. 縣市區域平均** | `_縣市區域平均.xlsx` | `縣市` \| `鄉鎮區` \| `總答對率` \| `向度明細` |
| **6. 不同家庭背景平均**| `_不同家庭背景平均.xlsx` | `縣市` \| `身分別` \| `總答對率` \| `向度明細` |
| **7. 縣市平均 (等級描述)** | `_縣市平均(等級描述).xlsx` | `縣市` \| `【扣除特殊生】到考、總答對率與三等級` \| `【含特殊生】到考、總答對率與三等級` |
| **8. 各校平均 (等級描述)** | `_各校平均(等級描述).xlsx` | `學校代碼` \| `學校名稱` \| `【扣除特殊生】到考、總答對率與三等級` \| `【含特殊生】到考、總答對率與三等級` |
| **9. 各班平均 (等級描述)** | `_各班平均(等級描述).xlsx` | `學校代碼` \| `班級代碼` \| `【扣除特殊生】到考、總答對率與三等級` \| `【含特殊生】到考、總答對率與三等級` |
| **10. 縣市區域平均 (等級描述)** | `_縣市區域平均(等級描述).xlsx` | `縣市` \| `鄉鎮區` \| `【扣除特殊生】到考、總答對率與三等級` \| `【含特殊生】到考、總答對率與三等級` |
| **11. 家庭背景平均 (等級描述)** | `_不同家庭背景平均(等級描述).xlsx` | `縣市` \| `身分別` \| `【扣除特殊生】到考、總答對率與三等級` \| `【含特殊生】到考、總答對率與三等級` |
| **12. 總平均 (等級描述)** | `_總平均(等級描述).xlsx` | `科目` \| `年級` \| `【扣除特殊生】到考、總答對率與三等級` \| `【含特殊生】到考、總答對率與三等級` |
| **7. 缺考名單** | `_缺考名單.xlsx` | 缺考學生名單（含兩種流水號） |
| **8. 個人成績含題數** | `_個人成績含題數.xlsx` | 個人分項答對題數 |
| **9. 全體名單成績** | `_全體名單成績含缺考.xlsx` | 含缺考之全體成績單 |
| **10. 個人成績** | `_個人成績.xlsx` | 原始作答、逐題得分與向度明細（含等級標記） |
| **11. 總平均** | `_總平均.xlsx` | `科目` \| `年級` \| `到考人數` \| **`精熟/基礎/待加強人數與率(%)`** \| `總平均` \| `向度明細` |
| **12. 試題 CTT 品質與診斷**| `_試題CTT品質與診斷.xlsx` | CTT 逐題品質預覽 |
| **13. 分析結果明細** | `_分析結果.xlsx` | 雙分頁（選項明細 + 逐題摘要） |
| **14. 試題分析總表** | `_試題分析總表.xlsx` | 傳統 27% 高低分組格式化美化 Excel |
| **15. 縣市三等級試題分析**| `_縣市三等級試題分析.xlsx` | **對齊臺中教大規範**（全體/精熟/基礎/待加強選答矩陣） |

---

## 🛠️ 修改指南與模組對照

| 想修改的內容 | 主要檔案 |
| :--- | :--- |
| 科目、年級、欄位別名、預覽選項 | [R/00_constants.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/00_constants.R) |
| 四捨五入、高精度答對率、共用工具 | [R/01_utils.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/01_utils.R) |
| 檔案合併、總流水號置於 Column A | [R/06_merge.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/06_merge.R) |
| 多維度向度解析 (如 `E41_向度`) 與驗證 | [R/10_validation.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/10_validation.R) |
| 檔名規則、單科／批次自動配對 | [R/20_io.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/20_io.R) |
| 答案判定、缺考定義、向度得分 | [R/30_scoring.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/30_scoring.R) |
| **27% CTT 與三等級試題分析計算引擎** | [R/35_ctt.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/35_ctt.R) |
| 到考/特殊生、平均、排名及 PR | [R/40_summaries.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/40_summaries.R) |
| 成績 Excel 報表內容與 ZIP 打包 | [R/50_exports.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/50_exports.R) |
| **CTT 與三等級格式化美化 Excel 匯出** | [R/55_ctt_exports.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/55_ctt_exports.R) |
| 工作結果欄位與耗時管理 | [R/60_jobs.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/60_jobs.R) |
| 成績分析頁上傳、檢查與背景計算 | [R/mod_run.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/mod_run.R) |
| 摘要卡、分布圖、成績預覽 | [R/mod_results.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/mod_results.R) |
| **獨立 CTT 與三等級試題分析頁籤模組** | [R/mod_ctt.R](file:///Users/liangchengyu/SAAA-itemAnalysis/R/mod_ctt.R) |
| 導覽頁、主題與整體框架 | [app.R](file:///Users/liangchengyu/SAAA-itemAnalysis/app.R) |

---

## 🧪 驗證與自動測試

專案包含完整的自動化測試腳本：

```bash
Rscript tests/smoke_test.R
Rscript tests/module_test.R
```

測試涵蓋：單科/批次探索、答案對齊、多向度解析、27% 高低分組演算法、縣市三等級試題分析計算、15 份 Excel 寫檔與 ZIP 壓縮。
