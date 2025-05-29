// This function runs when the window has finished loading
window.onload = (event) => {
  // Check if the user is on a mobile device
  let isMobile = /iPhone|iPad|iPod|Android/i.test(navigator.userAgent);
  
  if (isMobile) {
    // Get the first element with the class 'toast'
    let myAlert = document.querySelectorAll(".toast")[0];

    // If the toast element exists, create a new Bootstrap toast and show it
    if (myAlert) {
      let bsAlert = new bootstrap.Toast(myAlert);
      bsAlert.show();
    }

    // Get all elements with the class 'map' and the element with the id 'map_mobile'
    let mapElements = document.querySelectorAll(".map");
    let mapElement2 = document.getElementById("map_mobile");

    // Hide all map elements and show the mobile map element
    mapElements.forEach((mapElement) => {
      mapElement.style.display = "none";
      mapElement2.style.display = "block";
    });
  }
};

// Add an event listener to handle scrolling
window.addEventListener('scroll', function () {
  // Get all section elements
  const sections = document.querySelectorAll('section');
  let currentSection = ''; // Variable to keep track of the current section

  // Iterate through each section
  sections.forEach(section => {
    const sectionTop = section.offsetTop; // Get the distance from the top of the page to the section
    const sectionHeight = section.clientHeight; // Get the height of the section

    // Check if the page has been scrolled to the section
    if (pageYOffset >= sectionTop - sectionHeight / 3) {
      currentSection = section.getAttribute('id'); // Update the current section id
    }
  });

  // Update the navigation links to highlight the current section
  document.querySelectorAll('.nav-link').forEach(link => {
    link.classList.remove('active'); // Remove the 'active' class from all links
    if (link.getAttribute('href').slice(1) === currentSection) {
      link.classList.add('active'); // Add the 'active' class to the current section link
    }
  });
});

// Function to send an email
function sendEmail() {
  const emailInput = document.getElementById("exampleInputEmail1"); // Get the email input element
  const textareaInput = document.getElementById("exampleFormControlTextarea1"); // Get the textarea input element
  const topicSelect = document.getElementById('Question_selection'); // Get the topic selection element

  // Get the selected topic text from the dropdown
  let selectedTopicText = topicSelect.options[topicSelect.selectedIndex].text;
  
  if (selectedTopicText === "Select") {
    selectedTopicText = " "; // If no topic is selected, set it to a blank space
  }
  
  const emailAddress = emailInput.value; // Get the email address from the input
  const message = textareaInput.value; // Get the message from the textarea

  // Construct a mailto link with the email subject and body
  const mailtoLink = `mailto:claude.widmer@uzh.ch?subject=GEO454 - ${selectedTopicText} (${emailAddress}) &body=${encodeURIComponent(message)}`;

  // Open the default email client with the constructed mailto link
  window.location.href = mailtoLink;
}

// Function to open a Bootstrap collapse element and scroll to it
function openAndScrollToCollapse(collapseId) {
  const element = document.getElementById(collapseId); // Get the collapse element by id
  const collapse = new bootstrap.Collapse(element, {
      toggle: false // Do not toggle visibility (only show)
  });
  collapse.show(); // Show the collapse element
  element.scrollIntoView({ behavior: 'smooth' }); // Scroll to the collapse element smoothly
}


// Fix scroll problem on big computer: https://www.w3schools.com/howto/howto_css_smooth_scroll.asp#section2
$(document).ready(function(){
  // Add smooth scrolling to all links
  $("a").on('click', function(event) {

    // Make sure this.hash has a value before overriding default behavior
    if (this.hash !== "") {
      // Prevent default anchor click behavior
      event.preventDefault();

      // Store hash
      var hash = this.hash;

      // Using jQuery's animate() method to add smooth page scroll
      // The optional number (800) specifies the number of milliseconds it takes to scroll to the specified area
      $('html, body').animate({
        scrollTop: $(hash).offset().top
      }, 800, function(){

        // Add hash (#) to URL when done scrolling (default click behavior)
        window.location.hash = hash;
      });
    } // End if
  });
});

