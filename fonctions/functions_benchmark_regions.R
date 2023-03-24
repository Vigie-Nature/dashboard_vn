library(data.table)
library(ggplot2)
library(raster)
library(rgdal)
library(scales)

test=F


benchmark_regions=function(data_participation,CoucheRegion,MaxTrend=300
                           ,CorrectInversionCoordinates=T
                           ,FiltrerAnneesAbsurdes=T){
  
  Regions=shapefile(CoucheRegion
                    ,encoding="UTF-8",use_iconv=TRUE)
  
  
  if(CorrectInversionCoordinates){
    if(mean(data_participation$longitude,na.rm=T)>40){
      VraiLatitude=data_participation$longitude
      VraiLongitude=data_participation$latitude
      data_participation$latitude=VraiLatitude
      data_participation$longitude=VraiLongitude
      
      
      
    }
    
  }
  
  
  data_participation_loc=subset(data_participation,!is.na(data_participation$longitude))
  data_participation_loc=subset(data_participation_loc,!is.na(data_participation_loc$latitude))
  lost=nrow(data_participation)-nrow(data_participation_loc)
  print(paste(lost,"participations sans localisation"))
  
  coordinates(data_participation_loc)=c("longitude","latitude")
  proj4string(data_participation_loc) <- CRS("+init=epsg:4326") # WGS 84
  
  #GIre=spTransform(data_participation_loc,proj4string(Regions))
  GIre=data_participation_loc
  
  Reg=raster::intersect(GIre,Regions)
  if(FiltrerAnneesAbsurdes){
    Ndata=nrow(Reg)
    Reg=subset(Reg,Reg$annee_participation>1988)
    YearToday=as.numeric(substr(Sys.Date(),1,4))
    Reg=subset(Reg,Reg$annee_participation<=YearToday)
    Nsel=nrow(Reg)
    print(paste0(Ndata-Nsel," participations avec date absurde"))
  }
  if(nrow(Reg)==0)
  {
    stop("aucun point en France, vérifiez les coordonnées")
    
    
    
  }else{
    table(Reg$region)  
    AggRegion=aggregate(Reg$participation_id,by=list(Reg$region),length)
    #barplot(AggRegion$x~AggRegion$Group.1,horiz=T,las=2,cex.names=0.5
    #       ,main=unique(AllData$observatoire)[i])
    
    # p<-ggplot(data=AggRegion, aes(x=Group.1, y=x)) +
    #  geom_bar(stat="identity") +
    #  coord_flip()+ labs(title = unique(AllData$observatoire)[i]
    #  #                   , x = "Regions", y = "nb participations")
    #plot(p)
    #dynamique
    #hist(Reg$annee_participation)
    table(Reg$annee_participation)
    
    YearMean=round(mean(Reg$annee_participation,na.rm=T))
    YearOld=YearMean-(YearToday-YearMean)
    Reg$LastHalf=(Reg$annee_participation>=YearMean)
    Reg$Periode[(Reg$annee_participation>=YearMean)&(Reg$annee_participation<YearToday)]="Recent"
    Reg$Periode[(Reg$annee_participation==YearToday)]="EnCours"
    Reg$Periode[(Reg$annee_participation>=YearOld)&(Reg$annee_participation<YearMean)]="Old"
    Reg$Periode[(Reg$annee_participation<YearOld)]="VeryOld"
    table(Reg$Periode)
    AggRegionP=aggregate(Reg$participation_id,by=c(list(Reg$region),list(Reg$Periode))
                         ,length)
    AggRegionPc=dcast(as.data.table(AggRegionP),Group.1~Group.2)
    AggRegionPc[is.na(AggRegionPc)]=0
    
    AggRegionPc$Trend=(AggRegionPc$Recent/AggRegionPc$Old-1)*100
    AggRegionPc=subset(AggRegionPc,!is.na(AggRegionPc$Trend))
    #AggRegionPc$Trend[AggRegionPc$Trend>500]=500
    #barplot(AggRegionPc$Trend,names.arg=AggRegionPc$Group.1,horiz=T,las=2,cex.names=0.5
    #      ,main=unique(AllData$observatoire)[i])
    
    AggRegionPc$Group.1=factor(AggRegionPc$Group.1
                               ,levels=AggRegionPc$Group.1[order(AggRegionPc$Trend)])
    
    
    Ytitle=paste0("tendance de participation (",YearOld,"-",YearMean-1," vs. ",YearMean,"-"
                  ,YearToday-1,", en %)")
    p2<-ggplot(data=AggRegionPc, aes(x=Group.1, y=Trend)) +
      geom_bar(stat="identity") +
      coord_flip()+ labs(x = "Regions", y = Ytitle)+ 
      scale_y_continuous(limits=c(min(AggRegionPc$Trend-10),min(max(AggRegionPc$Trend)+10,MaxTrend)),oob = rescale_none)
    #+
    # ylim(c(min(AggRegionPc$Trend-10),min(max(AggRegionPc$Trend)+10,300)))
    #plot(p2)
    
    #regularité du trend ?
    AggRegionY=aggregate(Reg$participation_id,by=c(list(Reg$region),list(Reg$annee_participation))
                         ,length)
    AggRegionY$Group.1 <- factor(AggRegionY$Group.1, 
                                 levels=AggRegion$Group.1[order(AggRegion$x)] 
    )
    
    
    p1<-ggplot(data=AggRegionY, mapping=aes(x=Group.1,y=x)) +
      geom_bar(stat='identity',aes(fill=Group.2)) +
      coord_flip()+ labs( x = "Regions", y = "nb participations")+ 
      labs(fill = "Annees")
    #  layer(geom = "bar", mapping = aes(fill = cut))
    
    #plot(p1)
    
    
    
    AggRegionYDone=subset(AggRegionY,AggRegionY$Group.2!=YearToday)
    #AggRegionYDone_cast=dcast(AggRegionYDone,Group.1~Group.2,value.var="x")
    
    
    #################
    ##Turnover
    #################
    #YearList=unique(data_participation$annee_participation)
    #YearList=YearList[order(YearList)]
    #for (j in 2:length(YearList)){
    # dataPrevious=subset(data_participation,data_participation$annee_participation==YearList[i-1])
    #datai=subset(data_participation,data_participation$annee_participation==YearList[i])
    #  UserPrevious=unique(dataPrevious$user_id)
    # Useri=unique(datai$user_id)
    Output=list(p1,p2)
  }
  
  Output
  
}





if(test){
  AllData=fread("./data/data_vn.csv")
  table(AllData$observatoire)
  FileRegion="data/maps/region_shp/contours-geographiques-des-nouvelles-regions-metropole.shp"
  
  allplot=benchmark_regions(AllData,FileRegion)  
  
  
  for (i in 1:length(unique(AllData$observatoire))){
    print(unique(AllData$observatoire)[i])
    data_participation=subset(AllData,AllData$observatoire==unique(AllData$observatoire)[i])
    benchmark_regions(data_participation,CoucheRegion = FileRegion)  
  }    
}
