import "./style.css";
import gleamLogo from "./gleam.svg";
import viteLogo from "/vite.svg";
import { setup_counter } from "./counter.gleam";

declare module "./counter.gleam" {
  function setup_counter(btn: HTMLElement, counter: HTMLElement): null;
}

document.querySelector<HTMLDivElement>("#app")!.innerHTML = `
  <div>
    <a href="https://vitejs.dev" target="_blank">
      <img src="${viteLogo}" class="logo" alt="Vite logo" />
    </a>
    <a href="https://gleam.run" target="_blank">
      <img src="${gleamLogo}" class="logo gleam" alt="Gleam logo" />
    </a>
    <h1>Vite + Gleam</h1>
    <div class="card">
      <button id="counter" type="button">Count is <span id="count">0</span></button>
    </div>
    <p class="read-the-docs">
      Click on the Vite and Gleam logos to learn more
    </p>
  </div>
`;

setup_counter(
  document.querySelector<HTMLButtonElement>("#counter")!,
  document.querySelector<HTMLSpanElement>("#count")!,
);
