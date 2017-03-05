# Title
The Fixed Date  

\pagebreak

Load Packages and functions




Data Processing




# Introduction and Background

This is a citation [@Dickinson1999] and an in line citation @Dickinson1999

# Results

## A graph

this is a reference to the graph below which is called \ref(fig:fig1)



```r
ggplotly(ggplot(mtcars,aes(x=hp,y=mpg,color=factor(cyl)))+theme_light()+geom_point())
```

<div class="figure">
<!--html_preserve--><div id="htmlwidget-bce324aed93a9161aabc" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-bce324aed93a9161aabc">{"x":{"data":[{"x":[93,62,95,66,52,65,97,66,91,113,109],"y":[22.8,24.4,22.8,32.4,30.4,33.9,21.5,27.3,26,30.4,21.4],"text":["hp: 93<br>mpg: 22.8<br>factor(cyl): 4","hp: 62<br>mpg: 24.4<br>factor(cyl): 4","hp: 95<br>mpg: 22.8<br>factor(cyl): 4","hp: 66<br>mpg: 32.4<br>factor(cyl): 4","hp: 52<br>mpg: 30.4<br>factor(cyl): 4","hp: 65<br>mpg: 33.9<br>factor(cyl): 4","hp: 97<br>mpg: 21.5<br>factor(cyl): 4","hp: 66<br>mpg: 27.3<br>factor(cyl): 4","hp: 91<br>mpg: 26<br>factor(cyl): 4","hp: 113<br>mpg: 30.4<br>factor(cyl): 4","hp: 109<br>mpg: 21.4<br>factor(cyl): 4"],"key":null,"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"4","legendgroup":"4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text"},{"x":[110,110,110,105,123,123,175],"y":[21,21,21.4,18.1,19.2,17.8,19.7],"text":["hp: 110<br>mpg: 21<br>factor(cyl): 6","hp: 110<br>mpg: 21<br>factor(cyl): 6","hp: 110<br>mpg: 21.4<br>factor(cyl): 6","hp: 105<br>mpg: 18.1<br>factor(cyl): 6","hp: 123<br>mpg: 19.2<br>factor(cyl): 6","hp: 123<br>mpg: 17.8<br>factor(cyl): 6","hp: 175<br>mpg: 19.7<br>factor(cyl): 6"],"key":null,"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,186,56,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,186,56,1)"}},"hoveron":"points","name":"6","legendgroup":"6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text"},{"x":[175,245,180,180,180,205,215,230,150,150,245,175,264,335],"y":[18.7,14.3,16.4,17.3,15.2,10.4,10.4,14.7,15.5,15.2,13.3,19.2,15.8,15],"text":["hp: 175<br>mpg: 18.7<br>factor(cyl): 8","hp: 245<br>mpg: 14.3<br>factor(cyl): 8","hp: 180<br>mpg: 16.4<br>factor(cyl): 8","hp: 180<br>mpg: 17.3<br>factor(cyl): 8","hp: 180<br>mpg: 15.2<br>factor(cyl): 8","hp: 205<br>mpg: 10.4<br>factor(cyl): 8","hp: 215<br>mpg: 10.4<br>factor(cyl): 8","hp: 230<br>mpg: 14.7<br>factor(cyl): 8","hp: 150<br>mpg: 15.5<br>factor(cyl): 8","hp: 150<br>mpg: 15.2<br>factor(cyl): 8","hp: 245<br>mpg: 13.3<br>factor(cyl): 8","hp: 175<br>mpg: 19.2<br>factor(cyl): 8","hp: 264<br>mpg: 15.8<br>factor(cyl): 8","hp: 335<br>mpg: 15<br>factor(cyl): 8"],"key":null,"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(97,156,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(97,156,255,1)"}},"hoveron":"points","name":"8","legendgroup":"8","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text"}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[37.85,349.15],"ticktext":["100","200","300"],"tickvals":[100,200,300],"ticks":"outside","tickcolor":"rgba(179,179,179,1)","ticklen":3.65296803652968,"tickwidth":0.33208800332088,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(222,222,222,1)","gridwidth":0.33208800332088,"zeroline":false,"anchor":"y","title":"hp","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[9.225,35.075],"ticktext":["10","15","20","25","30","35"],"tickvals":[10,15,20,25,30,35],"ticks":"outside","tickcolor":"rgba(179,179,179,1)","ticklen":3.65296803652968,"tickwidth":0.33208800332088,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(222,222,222,1)","gridwidth":0.33208800332088,"zeroline":false,"anchor":"x","title":"mpg","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(179,179,179,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"annotations":[{"text":"factor(cyl)","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"modeBarButtonsToRemove":["sendDataToCloud"]},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script><!--/html_preserve-->
<p class="caption">\label{fig:fig1}A simple plot</p>
</div>





# Discussion




















\newpage


# Bibliography


