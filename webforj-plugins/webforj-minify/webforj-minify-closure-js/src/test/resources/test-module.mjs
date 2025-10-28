// ES6 Module example for testing .mjs minification
export class Square {
  constructor(ctx, length, x, y, color) {
    this.ctx = ctx;
    this.length = length;
    this.x = x;
    this.y = y;
    this.color = color;
  }

  draw() {
    this.ctx.fillStyle = this.color;
    this.ctx.fillRect(this.x, this.y, this.length, this.length);
    return { length: this.length, x: this.x, y: this.y, color: this.color };
  }

  reportArea() {
    return this.length * this.length;
  }

  reportPerimeter() {
    return this.length * 4;
  }
}

export const name = "square";

export default Square;
