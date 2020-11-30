
# 임대주택단지 조회 ---------------------------------------------------------------

gethouse = function(page = 1, region = '서울', supply = "행복", sz = 1000, api_key = NULL){

  # 업데이트 함수

  get.update = function(res){

    gs4_auth(email = 'aworklab@gmail.com')
    gs_test = '1nNlVfclGkXzn9P-t02FtQG0PM16rejiouII2JHc2P9U'
    sheet_append(gs_test, res, sheet = 'res_api_call')

  }
  # env에 저장 여부 체크 + 코드 데이터 호출


  if(!("code_api" %in% ls(globalenv()))){

    code_api = new.env()

    gs4_auth(email = 'aworklab@gmail.com')
    gs_test = '1nNlVfclGkXzn9P-t02FtQG0PM16rejiouII2JHc2P9U'

    code_api$town = read_sheet(gs_test, 'code_type')
    code_api$signgu = read_sheet(gs_test, 'code_signgu')

  }


  api_region = filter(code_api$town, type == "region" & name == region)$code
  api_spl = filter(code_api$town, type == "type_spl" & name == supply)$code
  api_sz = sz

  if(is.null(api_key)) {

    api_key = 'jwwSuH2HaiRIEecSHESiw%2FjQxp6EOhxnHmfyNRXz3BbpiBaXuPRKoBsAzjFSLdY%2FLy7CBLsoEJyVNFa3XMI2Xg%3D%3D'

  }

  api_url = 'http://apis.data.go.kr/B552555/lhLeaseInfo/lhLeaseInfo?serviceKey={api_key}&CNP_CD={api_region}&SPL_TP_CD={api_spl}&PG_SZ={api_sz}&PAGE={api_page}'

  # API 정보 수집

  res = NULL

  for(p in seq(page)){

    print(sprintf('%s %s : %s page is loading',
                  filter(code_api$town, type == "region" & name == region)$name_full,
                  filter(code_api$town, type == "type_spl" & name == supply)$name_full,
                  p          ))

    api_page = p


    # API CALL
    temp_raw = glue(api_url) %>% GET
    temp_content = content(temp_raw, as='text', encoding='UTF-8')

    parsing_time = with_tz(temp_raw$date, tzone = 'asia/seoul')



    # API 에러 여부 체크

    if(temp_raw$status_code %in% c(300, 400, 500)) {

      print(glue('Error status code : {temp_raw$status_code}'))

      res = tibble(

        idx = sprintf('%s_%s_%s',
                      str_remove_all(as.character(substring(parsing_time, 1, 10)), '-'),
                      api_spl,
                      temp_raw$status_code),
        log_date = parsing_time,
        type = supply,
        signgu_nm = region,
        region = NA,
        city = NA,
        name = temp_raw$status_code,
        date_move = NA,
        all_cnt = NA,
        hsh_total = NA,
        hsh_cnt = NA,
        size_excel = NA,
        deposit = NA,
        rent = NA

      )

      get.update(res)

      return(res)

    } else if(str_detect(temp_content, '오류')) {

      print(glue('current status code : {temp_raw$status_code} & 오류 발생'))

      res = tibble(

        idx = sprintf('%s_%s_%s',
                      str_remove_all(as.character(substring(parsing_time, 1, 10)), '-'),
                      api_spl,
                      temp_raw$status_code),
        log_date = parsing_time,
        type = supply,
        signgu_nm = region,
        region = NA,
        city = NA,
        name = temp_raw$status_code,
        date_move = NA,
        all_cnt = NA,
        hsh_total = NA,
        hsh_cnt = NA,
        size_excel = NA,
        deposit = NA,
        rent = NA

      )

      get.update(res)

      return(res)

    } else if(temp_raw$status_code == 200 &!str_detect(temp_content, '[가-힁]')) {

      print(glue('current status code : {temp_raw$status_code} & 내용없음' ))

      res = tibble(

        idx = sprintf('%s_%s_%s',
                      str_remove_all(as.character(substring(parsing_time, 1, 10)), '-'),
                      api_spl,
                      temp_raw$status_code),
        log_date = parsing_time,
        type = supply,
        signgu_nm = region,
        region = NA,
        city = NA,
        name = '내용 없음',
        date_move = NA,
        all_cnt = NA,
        hsh_total = NA,
        hsh_cnt = NA,
        size_excel = NA,
        deposit = NA,
        rent = NA

      )

      get.update(res)

      return(res)

    } else {

      print(glue('current status code : {temp_raw$status_code}'))

    }

    raw.data = fromJSON(temp_content)

    temp = raw.data[[2]][2] %>% reshape2::melt() %>% as_tibble
    temp = temp %>% select(num = L2, name = L3, value)
    temp = temp %>% group_by(num) %>% pivot_wider(names_from = 'name', values_from = 'value') %>%
      ungroup()

    n = c('num', 'size_excl', 'rent', 'hsh_total', 'd_num', 'name',
          'date_move', 'signgu_nm', 'hsh_cnt', 'all_cnt','type', 'deposit')

    temp %>%
      `colnames<-`(n) %>%
      mutate_at(vars(size_excl, rent, hsh_total, d_num, hsh_cnt, all_cnt, deposit), as.integer) %>%
      mutate(date_move = ymd(date_move),
             log_date = parsing_time) %>%
      # separate(location, c('region', 'city'), sep = ' ') %>%
      select( type, signgu_nm, log_date, name, date_move, all_cnt,
              hsh_total, hsh_cnt, size_excl, deposit, rent) -> temp

    temp = left_join(temp, code_api$signgu, by = 'signgu_nm')

    temp %>%
      mutate(idx = sprintf('%s_%s_%s',
                                   str_remove_all(substring(as.character(parsing_time), 1, 10), '-'),
                                   api_spl,
                                   signgu_code)) %>%
      select(idx, log_date, type, signgu_nm, region, city, everything()) -> temp



    res = bind_rows(res, temp)

  } # for 문 종료

  # gs 업로드 데이터 idx 중복 체크

  list_idx = unique(read_sheet(gs_test, 'res_api_call')$idx)
  res = res %>% filter(!(idx %in% list_idx))

  get.update(res)

  return(res)
}
