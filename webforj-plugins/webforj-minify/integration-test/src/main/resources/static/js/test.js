/**
 * Test JavaScript file for webforJ minify plugin
 * This file should be minified during the build process
 */

// Initialize the application
function init() {
  console.log("Application initialized");
  setupEventListeners();
}

// Setup event listeners for buttons
function setupEventListeners() {
  const buttons = document.querySelectorAll(".button");

  for (const button of buttons) {
    button.addEventListener("click", function() {
      console.log("Button clicked:", this.textContent);
      handleButtonClick(this);
    });
  }
}

// Handle button click events
function handleButtonClick(button) {
  const action = button.dataset.action;

  if (action === "submit") {
    submitForm();
  } else if (action === "cancel") {
    cancelForm();
  }
}

// Submit form handler
function submitForm() {
  console.log("Submitting form...");
  // Add form submission logic here
}

// Cancel form handler
function cancelForm() {
  console.log("Canceling form...");
  // Add cancel logic here
}

// Wait for DOM to be ready
document.addEventListener("DOMContentLoaded", init);
