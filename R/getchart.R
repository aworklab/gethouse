
# 현황 그리기 ------------------------------------------------------------------

getchart = function(){

  # GS 데이터 불러오기

  code_api = new.env()

  gs4_auth(email = 'aworklab@gmail.com')
  gs_test = '1A9t_LkPit8JT5Oodf6qBdkZcD50xaAlQPKe4lKvCrDU'

  code_api$town = read_sheet(gs_test, 'code_type')
  code_api$signgu = read_sheet(gs_test, 'code_signgu')

  # 중복값 체크

  df = read_sheet(gs_test, 'api_residence', col_types = c('cTcccccDiiiiiii')) %>% as.data.table
  df = df[!str_detect(name, '200|300|400|500'), .(idx, log_date, type, region, city, name, hsh_total)] %>% unique()
  df[, work_date := str_extract(idx, '\\d{8}') %>% ymd()]
  df[, code_signgu := str_extract(idx, '\\d{5}$')]
  df = df[!is.na(code_signgu),][order(type, code_signgu, name, -log_date)]
  df = df[df[, .I[1], by = .(type, code_signgu, name)]$V1, ]

  # 최종 공급 채수 계산

  cnt = df[, .(cnt = sum(hsh_total)), by = .(type, region, city)] %>% as_tibble

  # 레벨 순서 정리

  cnt$region = factor(cnt$region, levels = filter(code_api$town, type == "municipal")$name)

  # 차트생성

  cnt %>%
    ggplot(aes(x = region, y = cnt, fill = city)) +
    geom_col(color = '#333333', size = 0.1) +
    facet_grid(. ~ type) +
    xlab('') + ylab('채') +
    theme(legend.position = "bottom") -> res_gg

  res_gg

  return(res_gg)
}


