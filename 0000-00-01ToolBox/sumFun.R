macroSum.fun<-function(edbMon.ncol=156,
                       edbDay.ncol=22,
                       pert.nMonth=36,
                       mom.nMonth=9,
                       pert.nDay=60,
                       mom.nDay=90
){
  
  
  
  source(file.path(substr(getwd(),1,22),"header.R"))
  
  ## edb.month
  print("load edb.month  from ��ƵEDB.xls")
  edb.month<-read_excel(file.path(dataPath,"��ƵEDB.xls"),col_types=c("guess",rep("numeric",edbMon.ncol-1)))
  names(edb.month)[1]<-"����"
  
  edb.month <- edb.month%>%mutate(����=ymd(as.character(����)))
  edb.month<-edb.month[edb.month$����>as.Date("2005-1-1","%Y-%m-%d"),]
  
  edb.month<-xts(edb.month[,-1],order.by=edb.month[[1]])
  
  
  
  ## ָ�꿼�Ǽ��ִ�����ʽ
  # 1. ȫ��ʷ��λ�������ƹ�ֵ�������ǿ��Կ�����ʷ��λ���ģ���GDP����
  # 2. �ƶ���ʷ��λ��������һ�������ڵ�ָ�����ֵ������Ӧ��λ��һ�������ڣ�ƫ�߻�ƫ�Ͷ����쳣
  # 3. ���������ڶ������ڶ���
  
  ## ���
  # �߼��ع�GLM1��δ��X���������R%�ĸ��ʹ��ڸ�ָ��Ļع�
  # �߼��ع�GLM2��δ��X��������س�D%�ĸ��ʹ��ڸ�ָ��Ļع�
  # δ��X���������R%�����س�D%���龰��ɲ�λ�Ķ�ά���󣺻�����λ�龰������Ϊ�趨���ӷ���ƫ��
  # ʵ�ʲ�λ=������λ�龰����*(fun(prob(GLM1),prob(GLM2))),fun����ʹƽ���������С��˻���
  
  
  # ĳЩָ��ȱʧֵ��δ���
  ## ֮ǰû���ָ��
  ## �м�ĳһ��ʱ��û��ֵ
  ## �ۼ�ֵ��һ��û���������
  
  
  ## PMI
  
  ind.names<-c("PMI","PMI:����","PMI:�¶���")
  ind.id<-match(ind.names,names(edb.month))
  PMI.data<-edb.month['2005/',ind.id]
  
  ##ӯ�� 
  ### ��ҵ��ҵ:�����ܶ�:����ͬ�� 2012ǰʹ������֤ȯ����������
  ### ��ҵ��ҵ:��Ӫҵ������:�ۼ�ͬ�� û�е���ͬ�����ݣ�û��1������
  ### ��ҵ��ҵ:��Ӫҵ������:�ۼ�ͬ�� �� ��ҵ����ֵ:�ۼ�ͬ�ȣ����ϵ��0.93
  ### ��ҵ��ҵ:��Ӫҵ������:����ͬ�ȣ�ʹ������֤ȯ����������
  
  ind.names<-c("��ҵ����ֵ:����ͬ��","��ҵ��ҵ:��Ӫҵ������:����ͬ��","��ҵ��ҵ:�����ܶ�:����ͬ��")
  ind.id<-match(ind.names,names(edb.month))
  profit.data<-edb.month[,ind.id]
  
  
  ##Ͷ��
  ### ��Ʒ�������������ͬ�ȣ��ھ������仯��2005��7����2006��8��ʹ����һ��ȶ�Ӧ�·ݵ�����
  
  ind.names<-c("�̶��ʲ�Ͷ�ʵ���ͬ��",
               "�̶��ʲ�Ͷ��ʵ��ͬ��",
               "���̶��ʲ�Ͷ�ʵ���ͬ��",
               "����ҵͶ�ʵ���ͬ��",
               "����Ͷ�ʵ���ͬ��",
               "���ز�����Ͷ�ʵ���ͬ��",
               "�����¿����������ͬ��")
  ind.id<-match(ind.names,names(edb.month))
  invest.data<-edb.month[,ind.id]
  
  ##����
  ind.names<-c("�������Ʒ�����ܶ�:����ͬ��",
               "���۶�:������:����ͬ��",
               "���۶�:ʯ�ͼ���Ʒ��:����ͬ��",
               "���۶�:������װ�������:����ͬ��",
               "���۶�:�Ҿ���:����ͬ��",
               "��Ʒ�������������ͬ��")
  ind.id<-match(ind.names,names(edb.month))
  consum.data<-edb.month[,ind.id]
  
  XFPJ<-xts(apply(consum.data,1,mean,na.rm=T),order.by=index(consum.data))
  names(XFPJ) <- "����ƽ��"
  consum.data<-cbind(consum.data,XFPJ)
  
  
  
  ##ͨ��
  ind.names<-c("CPI:����ͬ��",
               "PPI:ȫ����ҵƷ:����ͬ��",
               "PPIRM:����ͬ��")
  ind.id<-match(ind.names,names(edb.month))
  inflation.data<-edb.month[,ind.id]
  
  ##�Ŵ�
  ind.names<-c("M1:ͬ��",
               "M2:ͬ��",
               "������ʹ�ģ����:ͬ��",
               "�г��ڴ������:����ͬ��")
  ind.id<-match(ind.names,names(edb.month))
  loan.data<-edb.month[,ind.id]
  ZSC<-loan.data[,1]-loan.data[,2]
  names(ZSC)<-"M1M2���ٲ�"
  loan.data<-cbind(loan.data,ZSC)
  
  ## �������ϻ����������
  macro.data<-merge(PMI.data,profit.data,invest.data,consum.data,inflation.data,loan.data)
  
  print("select macro indicators from edb.month:")
  print(names(macro.data))
  # sel.ind<-c(#  "PMI",
  #              "��ҵ��ҵ.�����ܶ�.����ͬ��",
  #         "��Ʒ�������������ͬ��",
  #         "�������Ʒ�����ܶ�.����ͬ��",
  #         #"�̶��ʲ�Ͷ�ʵ���ͬ��",
  #         "�����¿����������ͬ��",
  #        #"���۶�.������.����ͬ��",
  #        "CPI.����ͬ��",
  #        #"M2.ͬ��",
  #        "�г��ڴ������.����ͬ��"
  #       )
  # macro.data<-macro.data[,match.names(sel.ind,names(macro.data))]
  
  
  ## ���������ʷ��λ��
  print("Generate histpert,rpert,mom,diff for macro.month:")
  macro.pert<-sapply(as.zoo(macro.data),function(x) rollapplyr(x,nrow(macro.data),function(y) tail(percentile(y),1),partial=T),simplify = F)
  names(macro.pert)<-paste0(names(macro.pert),".hisPert")
  macro.pert<-xts(do.call(cbind,macro.pert),order.by=index(macro.data))
  
  ## ����������5���ƶ���ʷ��λ��
  macro.rpert<-sapply(as.zoo(macro.data),function(x) rollapplyr(x,pert.nMonth,function(y) tail(percentile(y),1),partial = T),simplify = F)
  names(macro.rpert)<-paste0(names(macro.rpert),".pert",pert.nMonth,"M")
  macro.rpert<-xts(do.call(cbind,macro.rpert),order.by=index(macro.data))
  
  ## ����������3�����ȶ��������ûع�б��*R����
  id<-grep("M1M2���ٲ�",names(macro.data))
  macro.mom<-sapply(macro.data[,-id],function(x) rollapply(x,mom.nMonth,lm.value),simplify = F)
  macro.mom <- xts(do.call(cbind,macro.mom),order.by=index(macro.data))
  names(macro.mom) <- paste0(names(macro.mom),".mom",mom.nMonth,"M")
  
  macro.diff<-sapply(macro.data,diff,simplify = F)
  macro.diff <- xts(do.call(cbind,macro.diff),order.by=index(macro.data))
  names(macro.diff) <- paste0(names(macro.diff),".diff")
  
  
  
  ## �ϲ�����ԭʼ���ݡ���λ���Ͷ���
  macro.out<-do.call(merge,list(macro.data,macro.pert,macro.rpert,macro.mom,macro.diff))
  
  
  
  #macro.out<-macro.mom
  # 
  # macro.out<-macro.out[,c("CPI.����ͬ��","�����¿����������ͬ��.����","�������Ʒ�����ܶ�.����ͬ��.����",
  #                         "��Ʒ�������������ͬ��.��ʷ��λ��","��Ʒ�������������ͬ��.����",
  #                         "��Ʒ�������������ͬ��.5���ƶ���ʷ��λ��","�������Ʒ�����ܶ�.����ͬ��.5���ƶ���ʷ��λ��",
  #                         "��ҵ��ҵ.�����ܶ�.����ͬ��.5���ƶ���ʷ��λ��",
  #                         "�г��ڴ������.����ͬ��.����")]
  
  macro.out<-data.frame(macro.out,mon=format(index(macro.out),"%Y-%m"),stringsAsFactors = F)
  row.names(macro.out)<-macro.out[,"mon"]
  print("summary for macro.month")
  print(summary(macro.out))
  
  ## edb.day
  print("load edb.day from ��ƵEDB.xls")
  edb.day<-read_excel(file.path(dataPath,"��ƵEDB.xls"),col_types=c("guess",rep("numeric",edbDay.ncol-1)))
  names(edb.day)[1]<-"����"
  edb.day <- edb.day%>%mutate(����=ymd(as.character(����)))
  edb.day<-edb.day[edb.day$����>as.Date("2005-1-1","%Y-%m-%d"),]
  edb.day<-xts(edb.day[-1,-1],order.by=edb.day[[1]][-1])
  
  sel.ind<-c(#  "PMI",
    "�м��:��Ԫ�������",
    "����:��ծ������:10��",
    #"��Ԫָ��",
    "��֤A��",
    "��ծ��ծ����������:10��","���м���Ѻʽ�ع���Ȩ����:7��"
  )
  edb.day<-edb.day[,match.names(sel.ind,names(edb.day))]
  LC<-edb.day[,"��ծ��ծ����������:10��"]-edb.day[,"����:��ծ������:10��"]
  names(LC)<-"������ծ����"
  
  
  edb.day<-cbind(edb.day,LC)
  print("select indicators from edb.day: ")
  print(names(edb.day))
  
  print("Generate histpert,rpert,mom,diff for macro.month")
  ## ���������ʷ��λ��
  macro2.pert<-sapply(as.zoo(edb.day),function(x) rollapplyr(x,nrow(edb.day),function(y) tail(percentile(y),1),partial=T),simplify = F)
  names(macro2.pert)<-paste0(names(macro2.pert),".hisPert")
  macro2.pert<-xts(do.call(cbind,macro2.pert),order.by=index(edb.day))
  
  ## ����������5���ƶ���ʷ��λ��
  macro2.rpert<-sapply(as.zoo(edb.day),function(x) rollapplyr(x,pert.nDay,function(y) tail(percentile(y),1),partial = T),simplify = F)
  names(macro2.rpert)<-paste0(names(macro2.rpert),".pert",pert.nDay,"D")
  macro2.rpert<-xts(do.call(cbind,macro2.rpert),order.by=index(edb.day))
  
  ## ����������3�����ȶ��������ûع�б��*R����
  macro2.mom<-sapply(edb.day,function(x) rollapply(x,mom.nDay,lm.value),simplify = F)
  macro2.mom <- xts(do.call(cbind,macro2.mom),order.by=index(edb.day))
  names(macro2.mom) <- paste0(names(macro2.mom),".mom",mom.nDay,"D")
  
  macro2.diff<-sapply(edb.day,diff,simplify = F)
  macro2.diff <- xts(do.call(cbind,macro2.diff),order.by=index(edb.day))
  names(macro2.diff) <- paste0(names(macro2.diff),".diff")
  
  
  
  
  ## �ϲ�����ԭʼ���ݡ���λ���Ͷ���
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