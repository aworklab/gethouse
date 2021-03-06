# gethouse 패키지 (Rthon 2020)

* Rthon 2020 패키지 경연 대회 참가 결과물
* 패키지 의존성 문제 및 디버깅 이슈 해결
* 구글 스프레드시트 쓰기 권한 문제로 인해 제 3자는 gethouse 기능이 작동하지 않을 수 있음

### 제작 목적

<img src= "https://user-images.githubusercontent.com/58664538/100576028-9b281f00-3320-11eb-8070-ea04ac65ea1f.png" width = 60%>

* [공공 데이터포털](https://www.data.go.kr/)에서 제공하는 API 호출 기능을 활용한 데이터 수집 및 저장 (gethouse)
* 수집 된 데이터를 그래프로 정리하여 제공(getchart)
* 물리적 시간 한계로 패키지의 기능 상세화 대신, 활용 목적 흐름의 실증에 더 포커스를 맞춤


### 패키지 소개

#### gethouse()

<img src = "https://user-images.githubusercontent.com/58664538/100575385-5059d780-331f-11eb-9c6c-f00201011d91.png" width = 60%>

* 국가 데이터 포털의 임대주택단지 리스트 데이터를 수집
* 광역 지자체 및 주택 공급 형태에 따라 개별적 데이터 수집 가능
* api key를 트래픽 한계로 사용 할 수 없을 경우 파라미터로 교체 가능
* 수집된 데이터는 [구글 스프레드시트](https://docs.google.com/spreadsheets/d/1A9t_LkPit8JT5Oodf6qBdkZcD50xaAlQPKe4lKvCrDU/edit?usp=sharing)로 구현된 약식 DB에 중복 체크 후 업데이트

<img src= "https://user-images.githubusercontent.com/58664538/100575462-7c755880-331f-11eb-83aa-a47bd657f845.png" width = 60%>


#### getchart()

<img src= "https://user-images.githubusercontent.com/58664538/100575206-f22cf480-331e-11eb-8a5d-8ac3675283d6.png" width = 40%>

* 구글 스프레드 시트에 저장된 데이터로 지역별 공급 주택 수를 막대 그래프로 표시
* 저장된 데이터를 지역 별로 일자 확인 후 최신 데이터만 합산
* 구체적 기능은 없으며, ggplot2로 막대 그래프로만 결과물 제공 

### 향후 계획 

* API 기능으로 수집할 수있는 공공 데이터 목록화
* 데이터 활용 폭에 대한 기획안 마련 및 수집 활동으로 DB 구축
* 구글 스프레드 시트 외 정상적인 DB 구축 방안 확인 (빅쿼리 등)
* 인사이트 도출이 가능한 부분으로의 방향성 강화 및 정보 제공 기능 추가
