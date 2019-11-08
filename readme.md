# < 4딸라 >조
![학기](https://img.shields.io/badge/%ED%95%99%EA%B8%B0-19--2-lightgrey) ![과목](https://img.shields.io/badge/%EA%B3%BC%EB%AA%A9-%EB%8D%B0%EC%9D%B4%ED%84%B0%EA%B3%B5%ED%95%99-blue) ![교수](https://img.shields.io/badge/%EA%B5%90%EC%88%98-%EC%9D%B4%EA%B4%91%EC%B6%98-9cf) ![조](https://img.shields.io/badge/%EC%A1%B0-4-orange) ![조원](https://img.shields.io/badge/%EC%A1%B0%EC%9B%90-%EC%9D%B4%ED%95%99%EB%AF%BC%20%EC%B5%9C%ED%98%B8%EC%8B%9D%20%EA%B9%80%ED%98%B8%EC%84%B1%20%EC%9D%B4%EC%A7%84%EC%9B%90-yellow) 

![Imgur](https://i.imgur.com/T1usFke.jpg)

> 프로젝트 주제

국회의원 선거 예측  

> 프로젝트 목표

대한민국 국회의원 선거('총선')을 선거구 마다 지니는 통계 자료를 활용하여 미래의 총선을 예측 해본다.

> 실행 방안

대한민국 모든 선거구를 하기 전에 목표를 '서울시' 지역의 선거구로 한정한다.

데이터는 2008년, 2012년, 2016년 총선 결과와 그 당시 년도의 선거구 지역과 관련된 지역별 통계 자료를 활용한다.

> 현재 진행 단계

* 조원들 모두 2008년, 2012년, 2016년 서울시 동별 자료를 가능한대로 모았다.

* 각 데이터들을 필요한 열만 남기고, 행과 열의 명칭과 관측치의 단위(Scale)를 통일 시켰다.

* 전처리한 데이터들을 합쳐서 모델에 이용할 데이터프레임을 만들었다. (현재 2016년도가 대상)

* 데이터 전체 관측치들을 train과 test로 나눈 후 train을 이용하여 한 가지 모델을 만들었다. (RandomForestRegression 이용하였음)

* test 데이터를 모델로 예측해본 결과 (새누리당 득표수를 예측하고자 하였다), 실제 test 데이터의 새누리당 득표수와 적합된 test 데이터의 편차제곱의 평균(MSE)을 구해봤다.

* 바로 전 단계의 결과를 통해서 변수들의 중요도를 파악해야 하는것과 다른 학습모델들을 적용 해봐야할 필요성을 알게되었다.

> 진행 Check List

- [x] 데이터 수집
- [x] 각자 맡은 데이터 전처리
- [x] 데이터 Merge
- [x] 랜덤포레스트 모델 1개 적용해보기
- [ ] 변수 중요도 분석하기
