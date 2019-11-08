# 19-2 데이터공학 < 4딸라 >조
*조원: 이학민 최호식 김호성 이진원*

> 프로젝트 주제

국회의원 선거 예측  


> 프로젝트 목표

대한민국 국회의원 선거('총선')을 선거구 마다 지니는 통계 자료를 활용하여 미래의 총선을 예측 해본다.

> 실행 방안

대한민국 모든 선거구를 하기 전에 목표를 '서울시' 지역의 선거구로 한정한다.

데이터는 2008년, 2012년, 2016년 총선 결과와 그 당시 년도의 선거구 지역과 관련된 지역별 통계 자료를 활용한다.

> 현재 진행 단계

조원들 모두 2008년, 2012년, 2016년 서울시 동별 자료를 가능한대로 모았다.

각 데이터들을 필요한 열만 남기고, 행과 열의 명칭과 관측치의 단위(Scale)를 통일 시켰다.

전처리한 데이터들을 합쳐서 모델에 이용할 데이터프레임을 만든다. (현재 2016년도가 대상)

> 진행 Check List

- [x] 데이터 수집
- [x] 각자 맡은 데이터 전처리
- [x] 데이터 Merge
- [x] 랜덤포레스트 모델 1개 적용해보기
- [ ] 변수 중요도 분석하기
