#数据模型M
FileDataModel<-function(file){
	data<-read.csv(file,header=FALSE)	#读取csv文件
	names(data)<-c("uid","nid","pref")	#增加列名

	user <- unique(data$uid)			#用户数组
	item <- unique(sort(data$nid))		#新闻数组(结果与排序无关)

	M <- matrix(0, length(user), length(item))
	uidx <- match(data$uid, user)		#用户序列
	nidx <- match(data$nid, item)		#新闻序列
	news <- cbind(uidx, nidx, pref=data$pref)		#原始数据矩阵
	for(n in 1:nrow(news)){
		M[news[n,][1],news[n,][2]]<-news[n,][3]			
	}
	dimnames(M)[[2]]<-item				#设置模型列名
	print("M:");print(M)
	M   								#返回数据模型矩阵
}
#用户相似度S
UserSimilarity<-function(M,min,factor){
	row<-nrow(M)
	S<-matrix(0,row,row)
	for(z1 in 1:row){
		for(z2 in 1:row){
			if(z1>z2){			#下三角
				m1<-sum(M[z1,]==M[z2,])	#用户z1，z2评分相同新闻总数	?
				m2<-sum(M[z1,]!=M[z2,])	#用户z1，z2评分不同新闻总数
				if(m1+m2==0){
					S[z1,z2]<-min
				}else{
					S[z1,z2]<-m1/(m1+m2)*(1-(factor/sqrt(m1+m2)))
				}
				#print(z1);print(z2);print(m1);print(m2);print(S[z1,z2])
			}
		}
	}
	ts<-t(S)
	w<-which(upper.tri(ts))
	S[w]<-ts[w]
	print("S:");print(S)
	S
}
#随时间推移，更新用户相似度US
UpdateSimilarity<-function(S,row,decay){
	for(col in 1:ncol(S)){
		S[row,col]<-S[row,col]-decay
	}
	print("US:");print(S)
	S
}
#最近邻矩阵N
NearestUserNeighborhood<-function(S,n){
	row<-nrow(S)
	N<-matrix(0,row,n)
	for(z1 in 1:row){
		for(z2 in 1:n){
			m<-which.max(S[,z1])
			N[z1,z2]<-m
			S[m,z1]<-0
		}
	}
	print("N:");print(N)
	N
}
#新闻的推荐值R
AdaptiveRecommender<-function(M,S,N){
	row<-nrow(M);col<-ncol(M)
	R<-matrix(0,row,col)
	for(z1 in 1:row){
		num<-which(M[z1,]==1)	#用户z1喜欢的新闻列
		for(z2 in num){
			for(z3 in N[z1,]){	#用户z1的最近邻
				R[z3,z2] <- R[z3,z2]+S[z3,z1]
			}
		}
	}
	print("R:");print(R)
	R
}
#新闻推荐列表Q
TipList<-function(R,n){
	row<-nrow(R)
	Q<-matrix(0,row,n)
	for(z1 in 1:row){
		for(z2 in 1:n){
			m<-which.max(R[z1,])
			if(R[z1,m]==0){
				Q[z1,z2]<-0
			}else{
				Q[z1,z2]<-m
				R[z1,m]<-0
			}
		}
	}
	print("Q:");print(Q)
	Q
}

#运行程序
FILE<-"E:\\R\\AdaptiveModel\\testCF.csv"	#原始数据文件
NEIGHBORHOOD_NUM <- 2		#最近邻个数S
RECOMMENDER_NUM <- 3		#最多推荐结果R
MIN_SIMILARITY <- 0.001		#用户之间的最小相似度ε
PENALTY_FACTOR <- 1   		#惩罚因子θ
USER_SATISFACTION <- 3		#用户满意度Δ
TIME_DECAY<-0.01            #兴趣减弱度𝜆

#初始化
M<-FileDataModel(FILE)
S<-UserSimilarity(M,MIN_SIMILARITY,PENALTY_FACTOR)
N<-NearestUserNeighborhood(S,NEIGHBORHOOD_NUM)
R<-AdaptiveRecommender(M,S,N)
Q<-TipList(R,RECOMMENDER_NUM)

#更新用户1的推荐分数
S<-UpdateSimilarity(S,1,TIME_DECAY)
N<-NearestUserNeighborhood(S,NEIGHBORHOOD_NUM)
R<-AdaptiveRecommender(M,S,N)
Q<-TipList(R,RECOMMENDER_NUM)
