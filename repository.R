


######server.R##########################

if (input$radio == "iris") {
  iris=data.frame("iris"= iris)
  compiled_iris<-iris[1:4]
  pca <-prcomp(compiled_iris,scale=FALSE)
  pca2d(pca,
        group = iris[,5],
        show.ellipses=TRUE,
        ellipse.ci=0.9,
        show.plane=TRUE,
        fancy= FALSE) 
} else if(input$radio == "DNase") {
  DNase=data.frame("DNase"= DNase)
  compiled_DNase<-DNase[2:3]
  pca <-prcomp(compiled_DNase,scale=FALSE)
  pca2d(pca,
        group = DNase[,1],
        show.ellipses=TRUE,
        ellipse.ci=0.9,
        show.plane=TRUE,
        fancy= FALSE) 
} else if (input$radio == "A") {
  DNase=data.frame("DNase"= DNase)
  compiled_DNase<-DNase[2:3]
  pca <-prcomp(compiled_DNase,scale=FALSE)
  pca2d(pca,
        group = DNase[,1],
        show.ellipses=TRUE,
        ellipse.ci=0.9,
        show.plane=TRUE,
        fancy= FALSE) 
} else {
  return(NULL)
}




####ui.R####################
radioButtons(inputId = "radio" ,label = "Data list",choices = c("iris", "DNase", "Good Morning")),