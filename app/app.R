# ==============================================================================
# Script Name: app.R (Final Debugged Edition)
# Purpose: AI Maturity Navigator 2026 - Fixed Sourcing & Vertical Alignment
# Logic: 為大於其細 (Precision), 執大象 (Clarity)
# ==============================================================================

# 1. 核心套件載入 (確保部署環境穩定) --------------------------------------------
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(bsicons)
library(scales)   # 必要程式包
library(munsell)  # 必要程式包

# 2. 雙語字典與戰略說明 (Strict Bilingual - No Citations) ----------------------
i18n <- list(
  en = list(
    title = "AI Maturity Navigator 2026",
    sidebar_title = "Strategic Inputs",
    lang_label = "Language Selection",
    industry_label = "Industry Sector",
    maturity_label = "AI Maturity Score",
    maturity_desc = "Composite index measuring internal process integration and governance.",
    roi_card = "Predicted ROI %",
    roi_info = "Expected financial return based on the strategic audit model.",
    archetype_card = "Digital Archetype",
    archetype_info = "Strategic categorization of organizational AI readiness.",
    frontier_title = "The Strategic Efficiency Frontier",
    frontier_desc = "This curve reveals the law of diminishing returns. ROI gains accelerate in the Evolver phase but plateau at the Governance Ceiling.",
    expert_title = "Expert Mandate & Governance",
    source = "Data Source: Fortune 500 AI Adoption Dataset via Kaggle (2020-2025).",
    model_desc = "Model: Quadratic Regression Analysis (R² ≈ 0.45).",
    legal = "Disclaimer: For strategic simulation only. Past performance is not indicative of future results.",
    tip_point = "Tipping Point"
  ),
  zh = list(
    title = "2026 AI 成熟度導航系統",
    sidebar_title = "戰略輸入參數",
    lang_label = "語言選擇",
    industry_label = "選擇產業別",
    maturity_label = "AI 成熟度得分",
    maturity_desc = "衡量企業內部流程整合與治理能力的綜合指標。",
    roi_card = "預測投資回報率 (ROI %)",
    roi_info = "基於戰略審計模型預測之財務回報率。",
    archetype_card = "數位階級標籤",
    archetype_info = "根據數位準備度進行的戰略分類。",
    frontier_title = "戰略效率邊界曲線",
    frontier_desc = "此曲線揭示了邊際收益遞減規律。回報在進化者階段加速，但在觸及治理天花板後趨於平緩。",
    expert_title = "專家指令與治理建議",
    source = "數據來源：Kaggle 平台之 Fortune 500 強企業 AI 數據集 (2020-2025)。",
    model_desc = "統計模型：二次項回歸分析 (解釋力 R² ≈ 0.45)。",
    legal = "合規聲明：本工具僅供戰略模擬參考，不代表實際獲利保證。",
    tip_point = "技術引爆點"
  )
)

# 3. UI 介面設計 (Professional Dark Theme) -------------------------------------
ui <- page_sidebar(
  theme = bs_theme(
    bg = "#0F172A", fg = "#F8FAFC", primary = "#FACC15",
    base_font = font_google("Inter")
  ),
  title = textOutput("app_title"),
  
  sidebar = sidebar(
    title = textOutput("sb_title"),
    radioButtons("lang", "Language / 語言", choices = c("English" = "en", "中文" = "zh"), selected = "zh"),
    selectInput("industry", "Industry", choices = c("Technology", "E-commerce", "Finance", "Telecom", "Healthcare", "Manufacturing")),
    
    # 級距為 5
    sliderInput("maturity", "Maturity Score", min = 0, max = 100, value = 60, step = 5),
    span(textOutput("maturity_explanation"), style = "font-size: 0.85rem; color: #CBD5E1;"),
    
    hr(),
    span(textOutput("footer_source"), style = "font-size: 0.8rem; color: #94A3B8;"),
    br(),
    span(textOutput("footer_model"), style = "font-size: 0.8rem; color: #94A3B8;"),
    br(),
    span(textOutput("footer_legal"), style = "font-size: 0.7rem; color: #64748B; font-style: italic;")
  ),
  
  layout_column_wrap(
    width = 1/2,
    # 數值指標卡 (維持大比例)
    value_box(
      title = tooltip(textOutput("card_roi_title"), textOutput("card_roi_info")),
      value = tags$span(style = "font-size: 3.5rem; font-weight: 800;", textOutput("roi_val")),
      showcase = bs_icon("graph-up-arrow", size = "4.5rem"),
      theme = "primary"
    ),
    value_box(
      title = tooltip(textOutput("card_arch_title"), textOutput("card_arch_info")),
      value = tags$span(style = "font-size: 2.5rem; font-weight: 600;", textOutput("archetype_val")),
      showcase = bs_icon("cpu", size = "4.5rem"),
      theme = "secondary"
    ),
    
    # 圖表區
    card(
      full_screen = TRUE,
      card_header(textOutput("frontier_head")),
      plotOutput("frontier_plot", height = "500px"),
      card_footer(textOutput("frontier_explanation"), style = "font-size: 0.95rem; color: #CBD5E1; border-top: 1px solid #334155;")
    ),
    
    # 專家建議區 (垂直 1-5 點排列)
    card(
      card_header(textOutput("expert_head")),
      card_body(
        uiOutput("dynamic_advice")
      )
    )
  )
)

# 4. Server 邏輯 ---------------------------------------------------------------
server <- function(input, output) {
  
  L <- reactive({ i18n[[input$lang]] })
  
  # 文字映射輸出
  output$app_title <- renderText({ L()$title })
  output$sb_title <- renderText({ L()$sidebar_title })
  output$maturity_explanation <- renderText({ L()$maturity_desc })
  output$footer_source <- renderText({ L()$source })
  output$footer_model <- renderText({ L()$model_desc })
  output$footer_legal <- renderText({ L()$legal })
  output$card_roi_title <- renderText({ L()$roi_card })
  output$card_roi_info <- renderText({ L()$roi_info })
  output$card_arch_title <- renderText({ L()$archetype_card }) # 已修正變數名
  output$card_arch_info <- renderText({ L()$archetype_info })
  output$frontier_head <- renderText({ L()$frontier_title })
  output$frontier_explanation <- renderText({ L()$frontier_desc })
  output$expert_head <- renderText({ L()$expert_title })
  
  res <- reactive({
    m <- input$maturity
    val <- 20.3 + (0.5 * m) - (0.003 * m^2)
    status <- case_when(
      m >= 80 ~ ifelse(input$lang == "en", "Leader (Strategic Lion)", "領先獅子 (Leader)"),
      m >= 45 ~ ifelse(input$lang == "en", "Evolver (Agile Monkey)", "進化猴子 (Evolver)"),
      TRUE ~ ifelse(input$lang == "en", "Laggard (Dormant Sloth)", "守舊樹懶 (Laggard)")
    )
    list(roi = round(val, 1), status = status)
  })
  
  output$roi_val <- renderText({ paste0(res()$roi, "%") })
  output$archetype_val <- renderText({ res()$status })
  
  output$frontier_plot <- renderPlot({
    df <- data.frame(m = seq(0, 100, 1))
    df$roi <- 20.3 + (0.5 * df$m) - (0.003 * df$m^2)
    
    ggplot(df, aes(x = m, y = roi)) +
      geom_line(color = "#FACC15", size = 2.5) +
      geom_point(aes(x = input$maturity, y = res()$roi), color = "#38BDF8", size = 10, stroke = 2, shape = 21, fill = "#38BDF8") + 
      annotate("text", x = 88, y = 38, label = L()$tip_point, color = "#FACC15", fontface = "bold", size = 5) +
      labs(x = L()$maturity_label, y = "Predicted ROI %") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#0F172A", color = NA),
        panel.background = element_rect(fill = "#1E293B", color = NA),
        text = element_text(color = "#E2E8F0", size = 16),
        axis.text = element_text(color = "#94A3B8"),
        panel.grid = element_line(color = "#334155")
      )
  })
  
  # 動態戰略建議 (修正 Markdown 垂直渲染邏輯)
  output$dynamic_advice <- renderUI({
    m <- input$maturity
    text_out <- if(m < 45) {
      if(input$lang == "en") {
        "1. Decouple transformation success from raw compute spending.  \n2. Prioritize process integration over blind scaling.  \n3. Focus on high-quality data hygiene as a foundational pillar.  \n4. Establish clear baseline metrics for digital governance.  \n5. Audit data siloes to prepare for future structural fluidity."
      } else {
        "1. 將轉型成功與純粹的算力支出脫鉤。  \n2. 優先強化流程整合，而非盲目擴張規模。  \n3. 專注於高品質的數據清洗，建立穩固基礎。  \n4. 為數位治理建立清晰的基準指標。  \n5. 審計數據孤島，為未來的靈活架構做準備。"
      }
    } else if (m >= 45 && m < 80) {
      if(input$lang == "en") {
        "1. Navigate the 'Volatility Zone' by anticipating erratic ROI.  \n2. Leverage cross-sector data to break internal silos.  \n3. Deploy adaptable technical stacks for higher agility.  \n4. Accelerate AI integration to cross the 80-point threshold.  \n5. Monitor decision noise closely during this growth phase."
      } else {
        "1. 穿越「高雜訊波動區」，預判並應對不穩定的 ROI。  \n2. 利用跨部門數據打破孤島，提升運作效率。  \n3. 部署具備高度適應性的技術棧以增加靈活性。  \n4. 加速 AI 流程整合，全力突破 80 分領跑者門檻。  \n5. 在成長階段嚴密監控決策雜訊對回報的影響。"
      }
    } else {
      if(input$lang == "en") {
        "1. Maintain the 'Leader's Moat' through consistent governance.  \n2. Identify the 'Governance Ceiling' where ROI begins to plateau.  \n3. Strictly align with ISO 42001 frameworks for maximum stability.  \n4. Shift focus from technical competition to operational accountability.  \n5. Prioritize algorithm transparency to mitigate long-term debt."
      } else {
        "1. 透過持續的治理鞏固「領先者護城河」。  \n2. 識別「治理天花板」，預判回報趨緩的臨界點。  \n3. 嚴格對齊 ISO 42001 框架以確保極致穩定性。  \n4. 將焦點從技術競爭轉向運營問責制。  \n5. 優先提升算法透明度，降低長期技術債風險。"
      }
    }
    markdown(text_out)
  })
}

# 5. 啟動應用程式 --------------------------------------------------------------
shinyApp(ui, server)