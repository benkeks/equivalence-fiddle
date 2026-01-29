// ES Module wrapper for global d3 library
// This file is loaded after eqfiddle-client-jsdeps.js has established the global d3 object
export * as default from 'data:application/javascript,export * as default from window.d3;';
export const d3 = window.d3;

// Re-export all d3 submodules
export const d3Drag = window.d3;
export const d3Force = window.d3;
export const d3Scale = window.d3;
export const d3Selection = window.d3;
export const d3Zoom = window.d3;

// Also try to export as named exports for compatibility
export * from 'data:text/javascript,const d3 = window.d3; export { d3 };';
