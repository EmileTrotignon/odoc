function createWebWorker() {
  var parts = document.location.href.split("/");
  parts[parts.length - 1] = search_url;
  var blobContents = ['importScripts("' + parts.join("/") + '");'];
  var blob = new Blob(blobContents, { type: "application/javascript" });
  var blobUrl = URL.createObjectURL(blob);

  var worker = new Worker(blobUrl);
  URL.revokeObjectURL(blobUrl);

  return worker;
}

var worker = createWebWorker();

document.querySelector(".search-bar").addEventListener("input", (ev) => {
  worker.postMessage(ev.target.value);
});


worker.onmessage = (e) => {
  let results = e.data;
  let search_result = document.querySelector(".search-result-inner");
  search_result.innerHTML = "";
  let f = (entry) => {
    /* entry */
    let container = document.createElement("a");
    container.href = base_url + entry.url;
    container.classList.add("search-entry", entry.kind.replace(" ", "-"));
    search_result.appendChild(container);

    /* kind */
    let kind = document.createElement("code");
    kind.innerText = entry.kind;
    kind.classList.add("entry-kind");
    container.appendChild(kind);

    /* content */
    /*let content = document.createElement("div");
    content.classList.add("entry-content");
    container.appendChild(content);
    */

    /* title */
    let title = document.createElement("code");
    title.classList.add("entry-title");
    container.appendChild(title);

    /* name */
    let prefixname = document.createElement("span");
    prefixname.classList.add("prefix-name");
    prefixname.innerText =
      entry.id
        .slice(0, entry.id.length - 1)
        .join(".") + (entry.id.length > 1 && entry.name != "" ? "." : "");
    title.appendChild(prefixname);

    let name = document.createElement("span");
    name.classList.add("entry-name");
    name.innerText = entry.id[entry.id.length - 1];
    title.appendChild(name);
    
    /* rhs */
    if (typeof entry.rhs !== typeof undefined) {
      let rhs = document.createElement("code");
      rhs.classList.add("entry-rhs");
      rhs.innerHTML = entry.rhs
      title.appendChild(rhs);
    }

    /* comment */
    let comment = document.createElement("div");
    comment.innerHTML = entry.doc;
    comment.classList.add("entry-comment");
    container.appendChild(comment);

  };
  results.map(f);
};
