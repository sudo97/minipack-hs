import { Hello_world } from "./dir/file.js";
import { name, basinga } from "./other_dir/other_file.js";

Hello_world();

console.log(name);
console.log(basinga());

export const a = 1;

export function f() {
  console.log(a);
}
