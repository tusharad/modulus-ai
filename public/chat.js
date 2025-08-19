function toggleSidebar() {
    const sidebar = document.getElementById("my-sidebar");
    sidebar.classList.toggle("collapsed");
    console.log("sidebar toggled");
}

function toggleNavbar() {
    //set att area-expand true/false in btn
    //toggle collapsed classItem in btn
    //toggle show classItem in divArea
    const toggleBtn = document.querySelector(".navbarToggleBtn");
    const divArea = document.querySelector(".navbarToggleDiv");
    if (toggleBtn.getAttribute("aria-expanded") === "true") {
        toggleBtn.setAttribute("aria-expanded", "false");
        toggleBtn.classList.add("collapsed");
        divArea.classList.remove("show");
    } else {
        toggleBtn.setAttribute("aria-expanded", "true");
        toggleBtn.classList.remove("collapsed");
        divArea.classList.add("show");
    }
}
