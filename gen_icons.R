
scriptPath <- function() {getSrcDirectory(scriptPath);}
setwd(scriptPath())
rm(list=ls())


xs = c("4682b4 ", "808080 ")
cnames = c("male", "male_last")

ys = c(-1,0,1,2,3,4,5, '?')
fnames = c(-1,0,1,2,3,4,5,'u')

for(i in 1:NROW(xs)){
  
  x = xs[i]
  
  
  for(j in 1: NROW(ys)) { 
    
    y = ys[j]
 
 text = sprintf('<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg
 xmlns:dc="http://purl.org/dc/elements/1.1/"
   xmlns:cc="http://creativecommons.org/ns#"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                xmlns:svg="http://www.w3.org/2000/svg"
                xmlns="http://www.w3.org/2000/svg"
                xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
                xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
                height="520"
                id="Layer_1"
                version="1.1"
                viewBox="0 0 392 520"
                width="392"
                xml:space="preserve"
                sodipodi:docname="icon_male_template.svg"
                inkscape:version="0.92.3 (2405546, 2018-03-11)"><metadata
                id="metadata13"><rdf:RDF><cc:Work
                rdf:about=""><dc:format>image/svg+xml</dc:format><dc:type
                rdf:resource="http://purl.org/dc/dcmitype/StillImage" /><dc:title /></cc:Work></rdf:RDF></metadata><defs
                id="defs11" /><sodipodi:namedview
                pagecolor="#ffffff"
                bordercolor="#666666"
                borderopacity="1"
                objecttolerance="10"
                gridtolerance="10"
                guidetolerance="10"
                inkscape:pageopacity="0"
                inkscape:pageshadow="2"
                inkscape:window-width="1920"
                inkscape:window-height="1137"
                id="namedview9"
                showgrid="false"
                inkscape:zoom="0.65186406"
                inkscape:cx="-382.94488"
                inkscape:cy="250.89563"
                inkscape:window-x="1912"
                inkscape:window-y="-8"
                inkscape:window-maximized="1"
                inkscape:current-layer="Layer_1" /><path
                d="m 101.62652,95.088574 c 23.0938,0.112106 41.81547,-18.609566 41.81547,-41.815476 0,-23.093799 -18.72167,-41.815471 -41.81547,-41.815471 -23.093804,0 -41.815476,18.721672 -41.815476,41.815471 0,23.093804 18.721672,41.815476 41.815476,41.815476 z"
                id="path2"
                inkscape:connector-curvature="0"
                style="stroke-width:1.12105823;fill:#%s;fill-opacity:1" /><g
                id="g6"
                transform="matrix(1.1210582,0,0,1.1210582,-185.36438,-24.416235)"><path
                d="M 293.4,115 H 256 218.6 C 190.4,115 172,139.8 172,163.4 V 277 c 0,22 31,22 31,0 V 172 h 6 v 285.6 c 0,30.4 42,29.4 43,0 V 293 h 7 1 v 164.7 c 1.7,31.2 43,28.2 43,-0.1 V 172 h 5 v 105 c 0,22 32,22 32,0 V 163.4 C 340,139.9 321.5,115 293.4,115 Z"
                id="path4"
                inkscape:connector-curvature="0"
                style="fill:#%s;fill-opacity:1" /></g><text
     xml:space="preserve"
        style="font-style:normal;font-weight:normal;font-size:216.79119873px;line-height:1.25;font-family:sans-serif;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;stroke-width:5.41978025"
        x="232.19034"
        y="204.08978"
        id="text4748"><tspan
        sodipodi:role="line"
        id="tspan4746"
        x="232.19034"
        y="204.08978"
        style="stroke-width:5.41978025">%s</tspan></text></svg>', x, x, y)
 
  filename = sprintf("www/set3/%s_%s.svg",fnames[j],cnames[i])
  print(filename)
  
  sink(filename)
  cat(text)
  sink()

  }
}


xs = c("FF6347", "808080")
cnames = c("female", "female_last")

ys = c(-1,0,1,2,3,4,5, '?')
fnames = c(-1,0,1,2,3,4,5,'u')

for(i in 1:NROW(xs)){
  
  x = xs[i]
  
  
  for(j in 1: NROW(ys)) { 
    
    y = ys[j]
    
    text = sprintf('<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg
                   xmlns:dc="http://purl.org/dc/elements/1.1/"
                   xmlns:cc="http://creativecommons.org/ns#"
                   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                   xmlns:svg="http://www.w3.org/2000/svg"
                   xmlns="http://www.w3.org/2000/svg"
                   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
                   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
                   height="520"
                   id="Layer_1"
                   version="1.1"
                   viewBox="0 0 392 520"
                   width="392"
                   xml:space="preserve"
                   sodipodi:docname="icon_female_template.svg"
                   inkscape:version="0.92.3 (2405546, 2018-03-11)"><metadata
                   id="metadata13"><rdf:RDF><cc:Work
                   rdf:about=""><dc:format>image/svg+xml</dc:format><dc:type
                   rdf:resource="http://purl.org/dc/dcmitype/StillImage" /><dc:title /></cc:Work></rdf:RDF></metadata><defs
                   id="defs11" /><sodipodi:namedview
                   pagecolor="#ffffff"
                   bordercolor="#666666"
                   borderopacity="1"
                   objecttolerance="10"
                   gridtolerance="10"
                   guidetolerance="10"
                   inkscape:pageopacity="0"
                   inkscape:pageshadow="2"
                   inkscape:window-width="1920"
                   inkscape:window-height="1137"
                   id="namedview9"
                   showgrid="false"
                   inkscape:zoom="1.28"
                   inkscape:cx="-42.044425"
                   inkscape:cy="273.13029"
                   inkscape:window-x="1912"
                   inkscape:window-y="-8"
                   inkscape:window-maximized="1"
                   inkscape:current-layer="Layer_1"
                   units="in" /><g
                   id="g6"
                   transform="matrix(1.1716847,0,0,1.1175338,-182.8139,-23.445605)"
                   style="fill:#%s;fill-opacity:1"><path
                   d="M 190.4,148.6 161,252.9 c -6.3,22.8 20.7,31.7 27.3,10.3 L 214.6,167 H 222 L 176.8,336 H 219 v 127 c 0,23 32,23 32,0 V 336 h 10 v 127 c 0,23 31,23 31,0 V 336 h 43.4 L 289.2,167 h 8.4 l 26.3,96.2 c 6.5,21.9 33.3,12.5 27.3,-10.2 L 321.8,148.6 c -4,-11.8 -18.2,-32.6 -42,-33.6 h -47.3 c -24.6,1 -38.7,21.6 -42.1,33.6 z"
                   id="path2"
                   inkscape:connector-curvature="0"
                   style="fill:#%s;fill-opacity:1" /><path
                   d="m 292.6,69.2 c 0,-20.6 -16.4,-37.3 -36.6,-37.3 -20.2,0 -36.6,16.7 -36.6,37.3 0,20.6 16.4,37.3 36.6,37.3 20.2,0 36.6,-16.7 36.6,-37.3 z"
                   id="path4"
                   inkscape:connector-curvature="0"
                   style="fill:#%s;fill-opacity:1" /></g><flowRoot
                   transform="matrix(8.1485792,0,0,7.7719819,-2774.8799,-271.55775)"
                   style="font-style:normal;font-weight:normal;font-size:40px;line-height:1.25;font-family:sans-serif;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none"
                   id="flowRoot834"
                   xml:space="preserve"><flowRegion
                   id="flowRegion830"><rect
                   y="30.283504"
                   x="369.02136"
                   height="134.79224"
                   width="127.05825"
                   id="rect828" /></flowRegion><flowPara
                   id="flowPara832" /></flowRoot><text
                    xml:space="preserve"
                    style="font-style:normal;font-weight:normal;font-size:216.79119873px;line-height:1.25;font-family:sans-serif;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;stroke-width:5.41978025"
                    x="232.19034"
                    y="204.08978"
                    id="text4748"><tspan
                    sodipodi:role="line"
                    id="tspan4746"
                    x="232.19034"
                    y="204.08978"
                    style="stroke-width:5.41978025">%s</tspan></text></svg>', x, x, x, y)
 
  filename = sprintf("www/set3/%s_%s.svg",fnames[j],cnames[i])
  print(filename)
  
  sink(filename)
  cat(text)
  sink()

  }
}


