const fs = require("fs");

const CENTRAL_DIRECTORY_SIGNATURE = 0x02014b50;
const END_OF_CENTRAL_DIRECTORY_SIGNATURE = 0x06054b50;
const END_OF_CENTRAL_DIRECTORY_SIZE = 22;
const MAX_ZIP_COMMENT_SIZE = 0xffff;

function findEndOfCentralDirectory(content) {
  const firstCandidate = content.length - END_OF_CENTRAL_DIRECTORY_SIZE;
  const lastCandidate = Math.max(
    0,
    firstCandidate - MAX_ZIP_COMMENT_SIZE
  );
  for (let offset = firstCandidate; offset >= lastCandidate; offset -= 1) {
    if (content.readUInt32LE(offset) === END_OF_CENTRAL_DIRECTORY_SIGNATURE &&
        offset + END_OF_CENTRAL_DIRECTORY_SIZE +
          content.readUInt16LE(offset + 20) === content.length) {
      return offset;
    }
  }
  throw new Error("ZIP end-of-central-directory record was not found.");
}

function parseZipEntryNames(content) {
  if (content.length < END_OF_CENTRAL_DIRECTORY_SIZE) {
    throw new Error("ZIP archive is shorter than its required footer.");
  }

  const footer = findEndOfCentralDirectory(content);
  const entryCount = content.readUInt16LE(footer + 10);
  const directorySize = content.readUInt32LE(footer + 12);
  const directoryOffset = content.readUInt32LE(footer + 16);
  if (entryCount === 0xffff || directorySize === 0xffffffff ||
      directoryOffset === 0xffffffff) {
    throw new Error("ZIP64 archives are not supported by this smoke check.");
  }
  const directoryEnd = directoryOffset + directorySize;
  if (directoryEnd > footer || directoryEnd > content.length) {
    throw new Error("ZIP central directory points outside the archive.");
  }

  const names = [];
  let cursor = directoryOffset;
  for (let index = 0; index < entryCount; index += 1) {
    if (cursor + 46 > directoryEnd ||
        content.readUInt32LE(cursor) !== CENTRAL_DIRECTORY_SIGNATURE) {
      throw new Error(`Invalid ZIP central-directory entry ${index + 1}.`);
    }
    const nameSize = content.readUInt16LE(cursor + 28);
    const extraSize = content.readUInt16LE(cursor + 30);
    const commentSize = content.readUInt16LE(cursor + 32);
    const nameStart = cursor + 46;
    const entryEnd = nameStart + nameSize + extraSize + commentSize;
    if (entryEnd > directoryEnd) {
      throw new Error(`ZIP entry ${index + 1} extends past the directory.`);
    }
    names.push(content.toString("utf8", nameStart, nameStart + nameSize));
    cursor = entryEnd;
  }
  if (cursor !== directoryEnd) {
    throw new Error("ZIP central-directory size does not match its entries.");
  }
  return names;
}

function readZipEntryNames(archivePath) {
  return parseZipEntryNames(fs.readFileSync(archivePath));
}

function makeSelfTestArchive(names) {
  const entries = names.map((name) => {
    const encodedName = Buffer.from(name, "utf8");
    const entry = Buffer.alloc(46 + encodedName.length);
    entry.writeUInt32LE(CENTRAL_DIRECTORY_SIGNATURE, 0);
    entry.writeUInt16LE(encodedName.length, 28);
    encodedName.copy(entry, 46);
    return entry;
  });
  const directory = Buffer.concat(entries);
  const footer = Buffer.alloc(END_OF_CENTRAL_DIRECTORY_SIZE);
  footer.writeUInt32LE(END_OF_CENTRAL_DIRECTORY_SIGNATURE, 0);
  footer.writeUInt16LE(names.length, 8);
  footer.writeUInt16LE(names.length, 10);
  footer.writeUInt32LE(directory.length, 12);
  footer.writeUInt32LE(0, 16);
  return Buffer.concat([directory, footer]);
}

if (require.main === module) {
  const expected = ["particle_summary.csv", "particle_details.csv"];
  const actual = parseZipEntryNames(makeSelfTestArchive(expected));
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error("ZIP central-directory self-test returned wrong entries.");
  }
  process.stdout.write("ZIP central-directory self-test passed.\n");
}

module.exports = { parseZipEntryNames, readZipEntryNames };
