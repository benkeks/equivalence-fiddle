package io.equiv.eqfiddle.spectroscopy.helpers

trait CytoscapeHelpers {
  
  def buildCytoscape(nodesString: String, edgesString: String) = s"""
<div id="cy" style="width: 100%; height: 300px;"></div>

<script type="module">
import cytoscape from "../../../node_modules/cytoscape/dist/cytoscape.esm.min.js";

var cy = cytoscape({
  container: document.getElementById('cy'),

  layout: {
    name: 'cose',
    padding: 10
  },

  style: cytoscape.stylesheet()
    .selector('node')
      .style({
        'content': 'data(name)',
        'text-valign': 'center'
      })
    .selector(':selected')
      .style({
        'border-width': 3,
        'border-color': '#333'
      })
    .selector('edge')
      .style({
        'opacity': 0.666,
        'curve-style': 'bezier',
        'target-arrow-shape': 'triangle',
        'label': 'data(label)'
      }),

  elements: {
    nodes: $nodesString,
    edges: $edgesString
  }
});
  </script>
  """
}
