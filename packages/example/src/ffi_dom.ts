export function qs(selector: string) {
  return document.querySelector(selector);
}

export function update<Prop extends keyof HTMLElement>(
  element: HTMLElement,
  property: Prop,
  updater: (v: HTMLElement[Prop]) => HTMLElement[Prop]
) {
  element[property] = updater(element[property]);
}

export function onclick(element: HTMLElement, handler: () => void) {
  element.addEventListener('click', handler);
}
