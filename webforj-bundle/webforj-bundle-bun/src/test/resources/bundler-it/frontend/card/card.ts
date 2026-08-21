import { LitElement, html } from 'lit';

export class ItCard extends LitElement {
  label: string = 'bundler-it-card';

  render() {
    return html`<span class="it-card">${this.label}</span>`;
  }
}

customElements.define('it-card', ItCard);
