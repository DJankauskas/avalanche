const stringCache = [""];

export function clear_string_cache() {
    stringCache.length = 1;
}

const stringChunkSize = 1024;
export function intern_string_at(string, pos) {
    let result = '';
    for (let i = 0; i < string.length; i += stringChunkSize) {
        result += String.fromCharCode(...string.subarray(i, i + stringChunkSize));
    }
    stringCache[pos] = result;
}

export function append_child(parent, child) {
    parent.append(child);
}

export function insert_child(parent, idx, child) {
    const afterIdx = parent.childNodes.item(idx);
    parent.insertBefore(child, afterIdx);
}

export function swap_children(parent, lesserIdx, greaterIdx) {
    const lesser = parent.childNodes.item(lesserIdx);
    const greater = parent.childNodes.item(greaterIdx);
    const afterGreater = greater.nextSibling;
    parent.replaceChild(greater, lesser);
    parent.insertBefore(lesser, afterGreater);
}

export function truncate_children(parent, len) {
    if (len === 0) {
        parent.innerHTML = '';
        return;
    }
    while (parent.childNodes.length > len) {
        parent.removeChild(parent.lastChild);
    }
}

export function create_text_node(strIdx) {
    return document.createTextNode(stringCache[strIdx]);
}


export function create_element(strIdx) {
    return document.createElement(stringCache[strIdx]);
}

export function set_text_content(textNode, strIdx) {
    textNode.textContent = stringCache[strIdx];
}

export function set_attribute(element, nameIdx, valueIdx) {
    const name = stringCache[nameIdx];
    const value = stringCache[valueIdx];
    switch (name) {
        case 'value':
            element.value = value;
            break;
        case 'checked':
            element.checked = value !== '';
            break;
        default:
            if (value === '') {
                element.removeAttribute(name);
            } else {
                element.setAttribute(name, value);
            }
    }
}
