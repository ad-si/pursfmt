import { execSync } from "node:child_process";

export const execSyncImpl = (cmd) => () => {
  try {
    const output = execSync(cmd, { encoding: "utf8", stdio: ["pipe", "pipe", "ignore"] });
    return { success: true, value: output };
  } catch (error) {
    return { success: false, value: error.message };
  }
};
