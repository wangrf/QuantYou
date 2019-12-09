macroSum.fun<-function(edbMon.ncol=156,
                       edbDay.ncol=22,
                       pert.nMonth=36,
                       mom.nMonth=9,
                       pert.nDay=60,
                       mom.nDay=90
){
  
  
  
  source(file.path(substr(getwd(),1,22),"header.R"))
  
  ## edb.month
  print("load edb.month  from 月频EDB.xls")
  edb.month<-read_excel(file.path(dataPath,"月频EDB.xls"),col_types=c("guess",rep("numeric",edbMon.ncol-1)))
  names(edb.month)[1]<-"日期"
  
  edb.month <- edb.month%>%mutate(日期=ymd(as.character(日期)))
  edb.month<-edb.month[edb.month$日期>as.Date("2005-1-1","%Y-%m-%d"),]
  
  edb.month<-xts(edb.month[,-1],order.by=edb.month[[1]])
  
  
  
  ## 指标考虑几种处理方式
  # 1. 全历史分位数，类似估值、动量是可以考虑历史分位数的，但GDP不行
  # 2. 移动历史分位数，假设一段区间内的指标绝对值理论上应该位于一个区间内，偏高或偏低都是异常
  # 3. 动量，短期动量或长期动量
  
  ## 框架
  # 逻辑回归GLM1，未来X天股市上涨R%的概率关于各指标的回归
  # 逻辑回归GLM2，未来X天股市最大回撤D%的概率关于各指标的回归
  # 未来X天股市上涨R%，最大回撤D%的情景组成仓位的二维矩阵：基础仓位情景矩阵，人为设定，视风险偏好
  # 实际仓位=基础仓位情景矩阵*(fun(prob(GLM1),prob(GLM2))),fun可以使平均、最大、最小或乘积等
  
  
  # 某些指标缺失值如何处理
  ## 之前没这个指标
  ## 中间某一段时间没有值
  ## 累计值，一月没有这个数据
  
  
  ## PMI
  
  ind.names<-c("PMI","PMI:生产","PMI:新订单")
  ind.id<-match(ind.names,names(edb.month))
  PMI.data<-edb.month['2005/',ind.id]
  
  ##盈利 
  ### 工业企业:利润总额:当月同比 2012前使用西南证券处理的数据
  ### 工业企业:主营业务收入:累计同比 没有当月同比数据，没有1月数据
  ### 工业企业:主营业务收入:累计同比 与 工业增加值:累计同比，相关系数0.93
  ### 工业企业:主营业务收入:当月同比，使用西南证券处理的数据
  
  ind.names<-c("工业增加值:当月同比","工业企业:主营业务收入:当月同比","工业企业:利润总额:当月同比")
  ind.id<-match(ind.names,names(edb.month))
  profit.data<-edb.month[,ind.id]
  
  
  ##投资
  ### 商品房销售面积当月同比，口径发生变化，2005年7月至2006年8月使用上一年度对应月份的数据
  
  ind.names<-c("固定资产投资当月同比",
               "固定资产投资实际同比",
               "民间固定资产投资当月同比",
               "制造业投资当月同比",
               "基建投资当月同比",
               "房地产开发投资当月同比",
               "房屋新开工面积当月同比")
  ind.id<-match(ind.names,names(edb.month))
  invest.data<-edb.month[,ind.id]
  
  ##消费
  ind.names<-c("社会消费品零售总额:当月同比",
               "零售额:汽车类:当月同比",
               "零售额:石油及制品类:当月同比",
               "零售额:建筑及装潢材料类:当月同比",
               "零售额:家具类:当月同比",
               "商品房销售面积当月同比")
  ind.id<-match(ind.names,names(edb.month))
  consum.data<-edb.month[,ind.id]
  
  XFPJ<-xts(apply(consum.data,1,mean,na.rm=T),order.by=index(consum.data))
  names(XFPJ) <- "消费平均"
  consum.data<-cbind(consum.data,XFPJ)
  
  
  
  ##通胀
  ind.names<-c("CPI:当月同比",
               "PPI:全部工业品:当月同比",
               "PPIRM:当月同比")
  ind.id<-match(ind.names,names(edb.month))
  inflation.data<-edb.month[,ind.id]
  
  ##信贷
  ind.names<-c("M1:同比",
               "M2:同比",
               "社会融资规模存量:同比",
               "中长期贷款余额:当月同比")
  ind.id<-match(ind.names,names(edb.month))
  loan.data<-edb.month[,ind.id]
  ZSC<-loan.data[,1]-loan.data[,2]
  names(ZSC)<-"M1M2增速差"
  loan.data<-cbind(loan.data,ZSC)
  
  ## 汇总以上基础宏观数据
  macro.data<-merge(PMI.data,profit.data,invest.data,consum.data,inflation.data,loan.data)
  
  print("select macro indicators from edb.month:")
  print(names(macro.data))
  # sel.ind<-c(#  "PMI",
  #              "工业企业.利润总额.当月同比",
  #         "商品房销售面积当月同比",
  #         "社会消费品零售总额.当月同比",
  #         #"固定资产投资当月同比",
  #         "房屋新开工面积当月同比",
  #        #"零售额.汽车类.当月同比",
  #        "CPI.当月同比",
  #        #"M2.同比",
  #        "中长期贷款余额.当月同比"
  #       )
  # macro.data<-macro.data[,match.names(sel.ind,names(macro.data))]
  
  
  ## 宏观数据历史分位数
  print("Generate histpert,rpert,mom,diff for macro.month:")
  macro.pert<-sapply(as.zoo(macro.data),function(x) rollapplyr(x,nrow(macro.data),function(y) tail(percentile(y),1),partial=T),simplify = F)
  names(macro.pert)<-paste0(names(macro.pert),".hisPert")
  macro.pert<-xts(do.call(cbind,macro.pert),order.by=index(macro.data))
  
  ## 宏观数据最近5年移动历史分位数
  macro.rpert<-sapply(as.zoo(macro.data),function(x) rollapplyr(x,pert.nMonth,function(y) tail(percentile(y),1),partial = T),simplify = F)
  names(macro.rpert)<-paste0(names(macro.rpert),".pert",pert.nMonth,"M")
  macro.rpert<-xts(do.call(cbind,macro.rpert),order.by=index(macro.data))
  
  ## 宏观数据最近3个季度动量（采用回归斜率*R方）
  id<-grep("M1M2增速差",names(macro.data))
  macro.mom<-sapply(macro.data[,-id],function(x) rollapply(x,mom.nMonth,lm.value),simplify = F)
  macro.mom <- xts(do.call(cbind,macro.mom),order.by=index(macro.data))
  names(macro.mom) <- paste0(names(macro.mom),".mom",mom.nMonth,"M")
  
  macro.diff<-sapply(macro.data,diff,simplify = F)
  macro.diff <- xts(do.call(cbind,macro.diff),order.by=index(macro.data))
  names(macro.diff) <- paste0(names(macro.diff),".diff")
  
  
  
  ## 合并以上原始数据、分位数和动量
  macro.out<-do.call(merge,list(macro.data,macro.pert,macro.rpert,macro.mom,macro.diff))
  
  
  
  #macro.out<-macro.mom
  # 
  # macro.out<-macro.out[,c("CPI.当月同比","房屋新开工面积当月同比.动量","社会消费品零售总额.当月同比.动量",
  #                         "商品房销售面积当月同比.历史分位数","商品房销售面积当月同比.动量",
  #                         "商品房销售面积当月同比.5年移动历史分位数","社会消费品零售总额.当月同比.5年移动历史分位数",
  #                         "工业企业.利润总额.当月同比.5年移动历史分位数",
  #                         "中长期贷款余额.当月同比.动量")]
  
  macro.out<-data.frame(macro.out,mon=format(index(macro.out),"%Y-%m"),stringsAsFactors = F)
  row.names(macro.out)<-macro.out[,"mon"]
  print("summary for macro.month")
  print(summary(macro.out))
  
  ## edb.day
  print("load edb.day from 日频EDB.xls")
  edb.day<-read_excel(file.path(dataPath,"日频EDB.xls"),col_types=c("guess",rep("numeric",edbDay.ncol-1)))
  names(edb.day)[1]<-"日期"
  edb.day <- edb.day%>%mutate(日期=ymd(as.character(日期)))
  edb.day<-edb.day[edb.day$日期>as.Date("2005-1-1","%Y-%m-%d"),]
  edb.day<-xts(edb.day[-1,-1],order.by=edb.day[[1]][-1])
  
  sel.ind<-c(#  "PMI",
    "中间价:美元兑人民币",
    "美国:国债收益率:10年",
    #"美元指数",
    "上证A股",
    "中债国债到期收益率:10年","银行间质押式回购加权利率:7天"
  )
  edb.day<-edb.day[,match.names(sel.ind,names(edb.day))]
  LC<-edb.day[,"中债国债到期收益率:10年"]-edb.day[,"美国:国债收益率:10年"]
  names(LC)<-"中美国债利差"
  
  
  edb.day<-cbind(edb.day,LC)
  print("select indicators from edb.day: ")
  print(names(edb.day))
  
  print("Generate histpert,rpert,mom,diff for macro.month")
  ## 宏观数据历史分位数
  macro2.pert<-sapply(as.zoo(edb.day),function(x) rollapplyr(x,nrow(edb.day),function(y) tail(percentile(y),1),partial=T),simplify = F)
  names(macro2.pert)<-paste0(names(macro2.pert),".hisPert")
  macro2.pert<-xts(do.call(cbind,macro2.pert),order.by=index(edb.day))
  
  ## 宏观数据最近5年移动历史分位数
  macro2.rpert<-sapply(as.zoo(edb.day),function(x) rollapplyr(x,pert.nDay,function(y) tail(percentile(y),1),partial = T),simplify = F)
  names(macro2.rpert)<-paste0(names(macro2.rpert),".pert",pert.nDay,"D")
  macro2.rpert<-xts(do.call(cbind,macro2.rpert),order.by=index(edb.day))
  
  ## 宏观数据最近3个季度动量（采用回归斜率*R方）
  macro2.mom<-sapply(edb.day,function(x) rollapply(x,mom.nDay,lm.value),simplify = F)
  macro2.mom <- xts(do.call(cbind,macro2.mom),order.by=index(edb.day))
  names(macro2.mom) <- paste0(names(macro2.mom),".mom",mom.nDay,"D")
  
  macro2.diff<-sapply(edb.day,diff,simplify = F)
  macro2.diff <- xts(do.call(cbind,macro2.diff),order.by=index(edb.day))
  names(macro2.diff) <- paste0(names(macro2.diff),".diff")
  
  
  
  
  ## 合并以上原始数据、分位数和动量
  macro2.out<-do.call(merge,list(edb.day,macro2.pert,macro2.rpert,macro2.mom,macro2.diff))
  
  macro2.out<-macro2.out[Tcalendar(macro2.out,daysInter = "monlast"),]
  macro2.out<-macro2.out['2005/',]
  
  macro2.out<-data.frame(macro2.out,mon=format(index(macro2.out),"%Y-%m"),stringsAsFactors = F)
  row.names(macro2.out)<-macro2.out[,"mon"]
  
  print("summary for macro.month")
  print(summary(macro2.out))
  
  macro.month<-macro.out
  macro.day<-macro2.out
  save(list=c("macro.month","macro.day"),file=file.path(sumPath,"macro.RData"))
  
  assign("macro.month",macro.month,envir = .GlobalEnv)
  assign("macro.day",macro.day,envir=.GlobalEnv)
  
  
}



symSum.fun<-function(sym,return.nDay=20,mom.nDay=60,value.nDay=120,price.nDay=60,value.nDay2=60){
  
  
  
  
  source(file.path(substr(getwd(),1,22),"header.R"))
  
  
  equity.syms=sym
  loadRData(equity.syms)
  x<-get(equity.syms)
  
  idx.close=match.names("close",colnames(x))
  
  xx<-which((as.numeric(x[,idx.close])==0))
  rnum<-max(length(xx),max(xx))
  
  assign(equity.syms,x[(rnum+1):nrow(x),],envir = .GlobalEnv)
  #assign(equity.syms,get(equity.syms)['2005/'])
  
  applyInd(sym = equity.syms,fun.name = "Tcalendar",
           arguments = list(x=get(equity.syms),daysInter="monlast"),
           label="month.last")
  
  applyInd(sym=equity.syms,fun.name="Return.calculateX",
           arguments=list(x=get(equity.syms),column="close"),
           label="dailyReturn")
  
  # applyInd(sym=equity.syms,fun.name="rollapplyX",
  #          arguments=list(x=get(equity.syms),column="dailyReturn",width=return.nDay,FUN="maxRunup",partial=T),
  #          label=paste0("maxUp",return.nDay))
  
  applyInd(sym=equity.syms,fun.name="rollapplyX",
           arguments=list(x=get(equity.syms),column="dailyReturn",width=return.nDay,FUN="Return.cumulative",partial=F),
           label=paste0("cum","Return",return.nDay))
  
  
  
  applyInd(sym=equity.syms,fun.name="leadX",
           arguments=list(x=get(equity.syms),column=paste0("cum","Return",return.nDay),k=return.nDay),
           label=paste0("RleadCum",return.nDay))
  
  
  applyInd(sym=equity.syms,fun.name="rollapplyX",
           arguments=list(x=get(equity.syms),column="dailyReturn",width=return.nDay,FUN="maxDrawdown",partial=T),
           label=paste0("maxDown",return.nDay))
  
  applyInd(sym=equity.syms,fun.name="leadX",
           arguments=list(x=get(equity.syms),column=paste0("maxDown",return.nDay),k=return.nDay),
           label=paste0("leadMaxD",return.nDay))
  
  
  applyInd(sym=equity.syms,fun.name="rollapplyX",
           arguments=list(x=get(equity.syms),column="close",width=mom.nDay,FUN="lm.value",partial=T),
           label=paste0("mom",mom.nDay))
  
  
  
  applyInd(sym=equity.syms,fun.name="rollapplyX",
           arguments=list(x=get(equity.syms),column="pe_ttm",width=value.nDay,FUN="last.percentile",partial=T),
           label=paste0("PE.","pert",value.nDay))
  
  applyInd(sym=equity.syms,fun.name="rollapplyX",
           arguments=list(x=get(equity.syms),column="close",width=value.nDay,FUN="last.percentile",partial=T),
           label=paste0("price.","pert",price.nDay))
  
  applyInd(sym=equity.syms,fun.name="rollapplyX",
           arguments=list(x=get(equity.syms),column="pe_ttm",width=nrow(get(equity.syms)),FUN="last.percentile",partial=T),
           label="PE.hisPert")
  
  applyInd(sym=equity.syms,fun.name="rollapplyX",
           arguments=list(x=get(equity.syms),column="dividendyield2",width=value.nDay,FUN="last.percentile",partial=T),
           label=paste0("div.","pert",value.nDay))
  applyInd(sym=equity.syms,fun.name="rollapplyX",
           arguments=list(x=get(equity.syms),column="dividendyield2",width=value.nDay2,FUN="last.percentile",partial=T),
           label=paste0("div.","pert",value.nDay2))
  applyInd(sym=equity.syms,fun.name="rollapplyX",
           arguments=list(x=get(equity.syms),column="dividendyield2",width=nrow(get(equity.syms)),FUN="last.percentile",partial=T),
           label="div.hisPert")
  
  save(list=c(equity.syms),file=file.path(sumPath,paste0(equity.syms,".RData")))
  
}
