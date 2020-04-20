vizmap = [

   {selector: "node", css: {
      "shape": "ellipse",
      "text-valign":"center",
      "text-halign":"center",
      "content": "data(name)",
      "border-color":"black","border-width":"1px",
       "width": 30,
       "height": 30,
       "background-color": "white",
      "font-size":"8px"}},

   {selector: "node[type='gene']", css: {
      "shape": "roundrectangle",
      "background-color": "beige"
       }},

   {selector: "node[type='protein']", css: {
      "shape": "roundrectangle",
      "background-color": "lightyellow"
       }},

   {selector: "node[type='metabolite'][assay <=0]", css: {
      "background-color": "mapData(assay, -1, 0, red, white)"
       }},

   {selector: "node[type='metabolite'][assay > 0]", css: {
      "background-color": "mapData(assay,  0,  1, white, blue)"
       }},

   {selector: "node[type='']", css: {
      "background-color": "lightblue"
       }},

   {selector: "node[type='bacterium']", css: {
      "shape": "heptagon",
      "background-color": "lightgreen"
       }},

   {selector: "node[type='clinical']", css: {
       "shape": "rectangle",
       "width": 80,
       "height": 30,
       "background-color": "lightblue"
       }},


   {selector:"node:selected", css: {
       "text-valign":"center",
       "text-halign":"center",
       "border-color": "black",
       "content": "data(id)",
       "border-width": "3px",
       "overlay-opacity": 0.5,
       "overlay-color": "lightgray"
       }},

   {selector: 'edge', css: {
      "line-color": "mapData(score, 0, 23, white, black)",
      "target-arrow-shape": "triangle",
      "target-arrow-color": "rgb(0, 0, 0)",
      "width": "2px",
      'curve-style': 'bezier'
      }},

    {selector: 'edge:selected', css: {
       "overlay-opacity": 0.4,
       "overlay-color": "lightgray"
        }},

   ];

