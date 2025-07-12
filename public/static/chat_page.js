function toggleSidebar() {
  const sidebar = document.getElementById("sidebar");
  sidebar.classList.toggle("collapsed");
}

// Handle window resize
window.addEventListener("resize", function () {
  if (window.innerWidth <= 768) {
    document.getElementById("sidebar").classList.add("collapsed");
  }
});

document.addEventListener("change", async function (e) {
  console.log("file upload detected");
  const fileInput = e.target;
  if (!(fileInput instanceof HTMLInputElement) || fileInput.id !== "document-upload") return;

  const form = fileInput.form;
  const sendBtn = document.getElementById("send-btn");
  if (!form || !sendBtn) return;
  console.log("reached here");
  const file = fileInput.files[0];
  if (!file || file.name === "") return;

  // Disable send button during upload
  sendBtn.disabled = true;

  // Remove existing hidden input if it exists
  const existing = form.querySelector("input[name='hasFile']");
  if (existing) existing.remove();

  try {
    const formData = new FormData();
    formData.append("file", file);

    const response = await fetch("/api/upload_attachment", {
      method: "POST",
      body: formData
    });

    if (!response.ok) {
      throw new Error("Upload failed");
    }

    const filePath = await response.text(); // Assume plain text file path

    const hiddenInput = document.createElement("input");
    hiddenInput.type = "hidden";
    hiddenInput.name = "hasFile";
    hiddenInput.value = filePath;
    form.appendChild(hiddenInput);

    console.log("Upload complete, file path:", filePath);
  } catch (err) {
    console.error("File upload error:", err);
    alert("Failed to upload file.");
    const hiddenInput = document.createElement("input");
    hiddenInput.type = "hidden";
    hiddenInput.name = "hasFile";
    hiddenInput.value = "";
    form.appendChild(hiddenInput);
  } finally {
    sendBtn.disabled = false;
  }
});
