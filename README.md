# Force directed graph layout in Elm
Implementation of [force directed layout algorithm](https://en.wikipedia.org/wiki/Force-directed_graph_drawing) in [Elm](http://elm-lang.org/).


The aim of force directed graph drawing algorithms is to produce aesthetically
pleasing layout of graphs by simulating attractive / repulsive forces among
nodes of the graph.

## See it in action [here](http://janhrcek.cz/elm-graph-layout.html)
Things you can try:
- Several example graphs
- Move nodes using drag and drop
- Tweak layout algorithm parameters using sliders

Note this implementation is still experimental, but I'd like to make this into reusable library soon.

## TODOs

- [x] Add possibility to tweak layout algorithm parameters in UI
- [x] Add sample graphs
- [x] Add node drag & drop
- [ ] Implement random graph (tree?) generation
- [ ] Make Physics simulation code more readable
- [ ] linear-algebra should not be needed - implement the primitives in separate module
- [ ] Add parameter explanation hints
- [ ] Make node / edge rendering logic configurable
- [ ] Enable LayoutGraph initialization without randomization command (perhaps just with random gen seed?)
