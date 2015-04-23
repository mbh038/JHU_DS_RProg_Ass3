best<-function(state,outcome){
        ## Read outcome data
        
        outcome.dat<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        nr<-nrow(outcome.dat)
        nc<-ncol(outcome.dat)
        u.state<-unique(outcome.dat[,7])
        n.state=length(u.state)
        u.outcome<-c("heart attack","heart failure","pneumonia")
        n.outcome=length(u.outcome)
        
        ## Check that state and outcome are valid
        #Check state
        st.test<-logical()
       for(i in 1:n.state){
               st.test[i]=identical(state,u.state[i])
       }
        if(sum(st.test)==0) {stop("invalid state")}

        #Check outcome
        oc.test<-logical()
        for(i in 1:n.outcome){
                oc.test[i]=identical(outcome,u.outcome[i])
        }
        if(sum(oc.test)==0) {stop("invalid outcome")}

        ## Return hospital name in that state with lowest 30-day death
        ## rate

        if(outcome=="heart attack"){
                col<-11
        }
        else if (outcome=="heart failure"){
                col<-17
        }
        else if (outcome=="pneumonia"){
                col<-23    
        }

        #create a data frame with 3 columns (State, Hospital, The required outcome)
        #first remove rows with missing values
        ss<-outcome.dat[outcome.dat[,7]==state & outcome.dat[,col]!="Not Available",]
        
        ss.df<-data.frame(ss[,7],as.character(ss[,2]),as.numeric(ss[,col]))
        colnames(ss.df)<-c("State","Hospital","Mortality_Rate")
        sorted.ss<-ss.df[with(ss.df,order(Mortality_Rate,Hospital)),]
        best<-sorted.ss[1,2]
        as.character(best)

}
