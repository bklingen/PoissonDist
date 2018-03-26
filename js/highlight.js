function highlight(instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);
  td.style.color = 'black';             
  if (instance.params) {
    mhrows = instance.params.index;
    mhrows = mhrows instanceof Array ? mhrows : [mhrows];
  }
  if (instance.params && mhrows.includes(row)) td.style.background = '#B2DF8A';
  if (value =='NA') {
    value = '';
    Handsontable.renderers.getRenderer('text')(instance, td, row, col, prop, value, cellProperties);
  }
}