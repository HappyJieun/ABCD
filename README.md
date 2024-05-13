# Fitbit 생체시계 설문조사 데이터 전처리


> Period: 2021.12 ~ 2022.01
>  
> Role: 전처리, 시각화



##### 22.01.16 last edit
---

## 0. Environment

+ Language : R

+ Editor : RStudio
---
## 1. Introduction

**Background**

청소년기의 생체리듬이 신체와 정신 건강에 미치는 영향을 확인하고자 함. 

---
## 2. Data Set

**Dataset Info.**

https://nda.nih.gov/data-structure/abcd_fbdpas01

<br/>

**File count**

8개의 csv 파일


---
## 3. Summary

**(1) Data Preprocessing**

- Fitbit 이용자의 설문 조사 데이터 전처리 및 시각화 담당
- 전처리: Variable Selection, correlation coefficient
- 시각화: Daily physical activity summaries

<br/>

**(2) Result**

- Fitbit 데이터와 설문조사 데이터를 병합하여 빅데이터 형성
- 설문조사 정보와 실제 데이터 간의 연관성 발견
- 시각화 지표 153개 생성
