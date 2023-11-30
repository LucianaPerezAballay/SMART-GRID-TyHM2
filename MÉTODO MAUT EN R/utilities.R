external_local_relations<-function(x) { 
	f<-0
	if ( x >=0 && x <=1 ) {
		f<-(1)*x + (0)
	} else if ( x >= 1 && x < 5 ) {
		f<-(0)*x + (1)
	} else if ( x >= 5 ) {
		f<-(0)*5 + (1)
	} 
	f<-max(0.0,f)
	f<-min(1.0,f)
	return(f)
}

project<-function(x) { 
	f<-0
	if ( x >=1 && x <=2 ) {
		f<-(-0.5)*x + (2)
	} else if ( x >= 2 && x < 3 ) {
		f<-(-0.5)*x + (1.5)
	} else if ( x >= 3 ) {
		f<-(-0.5)*3 + (1.5)
	} 
	f<-max(0.0,f)
	f<-min(1.0,f)
	return(f)
}

scope_capabilities<-function(x) { 
	f<-0
	if ( x >=0 && x <=4 ) {
		f<-(-1.225)*exp( -(0.2824)*x ) + (1.225)
	} else if ( x >= 4 && x < 13 ) {
		f<-(0)*x + (1)
	} else if ( x >= 13 ) {
		f<-(0)*13 + (1)
	} 
	f<-max(0.0,f)
	f<-min(1.0,f)
	return(f)
}

self_implementation<-function(x) { 
	f<-0
	if ( x >=1 && x <=2 ) {
		f<-(-0.5)*x + (3)
	} else if ( x >= 2 && x < 3 ) {
		f<-(-0.5)*x + (1)
	} else if ( x >= 3 ) {
		f<-(-0.5)*3 + (1)
	} 
	f<-max(0.0,f)
	f<-min(1.0,f)
	return(f)
}
