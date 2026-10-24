#import "Basic";
#import "String";

// File_Utilities.copy_directory recreates files with the process defaults.
// This wrapper additionally preserves Unix permission bits, including the
// executable bits required by packaged programs and helper executables.
copy_directory :: (source: string, destination: string) -> bool {
    if !File_Utilities.copy_directory(source, destination) return false;

    #if OS_IS_UNIX {
        Directory_Pair :: struct {
            source: string;
            destination: string;
        }
        Copy_Context :: struct {
            source_root: string;
            destination_root: string;
            directories: [..] Directory_Pair;
            success := true;
        }

        copy_context: Copy_Context;
        copy_context.source_root = source;
        copy_context.destination_root = destination;
        copy_context.directories.allocator = temp;

        visitor :: (info: *File_Utilities.File_Visit_Info, copy_context: *Copy_Context) {
            if !copy_context.success return;

            relative_name := advance(info.full_name, copy_context.source_root.count);
            destination_name := tprint("%%", copy_context.destination_root, relative_name);
            if info.is_directory {
                array_add(*copy_context.directories, .{
                    source = copy_temporary_string(info.full_name),
                    destination = copy_temporary_string(destination_name),
                });
                return;
            }

            copy_context.success = copy_unix_permissions(info.full_name, destination_name);
        }

        visit_ok := File_Utilities.visit_files(
            source,
            recursive = true,
            *copy_context,
            visitor,
            visit_directories = true,
        );
        if !visit_ok || !copy_context.success return false;

        // Apply directory modes from the leaves upward so restrictive parent
        // permissions cannot prevent us from reaching their children.
        directory_index := copy_context.directories.count;
        while directory_index > 0 {
            directory_index -= 1;
            pair := copy_context.directories[directory_index];
            if !copy_unix_permissions(pair.source, pair.destination) return false;
        }
        if !copy_unix_permissions(source, destination) return false;
    }

    return true;
}

#if OS_IS_UNIX {
    copy_unix_permissions :: (source: string, destination: string) -> bool {
        source_status: POSIX.stat_t;
        if POSIX.stat(temp_c_string(source), *source_status) != 0 {
            log_error("Could not read Unix permissions from %.\n", source);
            return false;
        }

        // Permission, set-id, and sticky bits; exclude the file-type bits.
        permission_bits := source_status.st_mode & cast(POSIX.mode_t) 0xFFF;
        if POSIX.chmod(temp_c_string(destination), permission_bits) != 0 {
            log_error("Could not apply Unix permissions from % to %.\n", source, destination);
            return false;
        }
        return true;
    }
}

File_Utilities :: #import "File_Utilities";
#if OS_IS_UNIX POSIX :: #import "POSIX";

path_char_equal :: (a: u8, b: u8) -> bool {
    if a >= #char "A" && a <= #char "Z" a += #char "a" - #char "A";
    if b >= #char "A" && b <= #char "Z" b += #char "a" - #char "A";
    if a == b return true;
    if (a == #char "/" || a == #char "\\") && (b == #char "/" || b == #char "\\") return true;
    return false;
}

path_equal :: (a: string, b: string) -> bool {
    if a.count == b.count {
        for i: 0..a.count-1 {
            if !path_char_equal(a[i], b[i]) return false;
        }
        return true;
    }

    longer := a;
    shorter := b;
    if b.count > a.count {
        longer = b;
        shorter = a;
    }

    start := longer.count - shorter.count;
    if start > 0 && !path_char_equal(longer[start - 1], #char "/") return false;

    for i: 0..shorter.count-1 {
        if !path_char_equal(longer[start + i], shorter[i]) return false;
    }

    return true;
}

path_is_under_directory :: (path: string, directory: string) -> bool {
    if path_equal(path, directory) return true;
    if path.count <= directory.count return false;

    for i: 0..directory.count-1 {
        if !path_char_equal(path[i], directory[i]) return false;
    }

    return path_char_equal(path[directory.count], #char "/");
}

/*
Takes a path string and makes it consistent enough for local file operations
and path comparisons.

- Empty paths stay empty.
- A leading "./" or ".\" is stripped.
- MSYS2 drive paths like "/c/Users/name/project" become "C:/Users/name/project".
- Windows drive paths like "C:\Users\name" are kept, with slashes normalized.
- Rooted slash paths like "/tmp/file" are kept rooted.
- Relative paths are joined onto cwd.
- Backslashes are converted to forward slashes.
- Simple "/./" segments are removed.

This does not fully canonicalize the path: it does not resolve "..", symlinks,
short names, casing, or ask the OS whether the path exists.
*/
resolve_path :: (path: string, cwd: string) -> string {
    if path.count == 0 return path;

    raw := path;
    if raw.count >= 2 && raw[0] == #char "." && (raw[1] == #char "/" || raw[1] == #char "\\") {
        raw = slice(raw, 2, raw.count - 2);
    }

    result: string;
    if raw.count >= 3 && raw[0] == #char "/" && raw[2] == #char "/" &&
       ((raw[1] >= #char "a" && raw[1] <= #char "z") || (raw[1] >= #char "A" && raw[1] <= #char "Z")) {
        result = tprint("%", raw);
        result[0] = raw[1];
        if result[0] >= #char "a" && result[0] <= #char "z" result[0] -= #char "a" - #char "A";
        result[1] = #char ":";
    } else if raw.count >= 2 && raw[1] == #char ":" {
        result = tprint("%", raw);
    } else if raw[0] == #char "/" || raw[0] == #char "\\" {
        result = tprint("%", raw);
    } else {
        result = tprint("%/%", cwd, raw);
    }

    for i: 0..result.count-1 {
        if result[i] == #char "\\" result[i] = #char "/";
    }

    cleaned: String_Builder;
    i := 0;
    while i < result.count {
        if result[i] == #char "/" && i + 2 < result.count && result[i + 1] == #char "." && result[i + 2] == #char "/" {
            append(*cleaned, "/");
            i += 3;
            continue;
        }

        append(*cleaned, result[i]);
        i += 1;
    }

    result = builder_to_string(*cleaned);
    return result;
}

File_Size_Unit_System :: enum {
    BINARY;  // KiB, MiB, GiB, ...
    DECIMAL; // KB, MB, GB, ...
}

File_Size_Unit :: enum {
    BYTE;
    KILO;
    MEGA;
    GIGA;
    TERA;
}

file_size_unit_multiplier :: (unit: File_Size_Unit, unit_system := File_Size_Unit_System.BINARY) -> u64 {
    base := ifx unit_system == .BINARY then cast(u64) 1024 else cast(u64) 1000;

    if unit == .BYTE return 1;
    if unit == .KILO return base;
    if unit == .MEGA return base * base;
    if unit == .GIGA return base * base * base;
    return base * base * base * base;
}

bytes_to_file_size :: (bytes: u64, unit: File_Size_Unit, unit_system := File_Size_Unit_System.BINARY) -> float64 {
    return cast(float64) bytes / cast(float64) file_size_unit_multiplier(unit, unit_system);
}

bytes_to_kilobytes :: (bytes: u64) -> float64 {
    return bytes_to_file_size(bytes, .KILO, .DECIMAL);
}

bytes_to_megabytes :: (bytes: u64) -> float64 {
    return bytes_to_file_size(bytes, .MEGA, .DECIMAL);
}

bytes_to_gigabytes :: (bytes: u64) -> float64 {
    return bytes_to_file_size(bytes, .GIGA, .DECIMAL);
}

bytes_to_terabytes :: (bytes: u64) -> float64 {
    return bytes_to_file_size(bytes, .TERA, .DECIMAL);
}

bytes_to_kibibytes :: (bytes: u64) -> float64 {
    return bytes_to_file_size(bytes, .KILO, .BINARY);
}

bytes_to_mebibytes :: (bytes: u64) -> float64 {
    return bytes_to_file_size(bytes, .MEGA, .BINARY);
}

bytes_to_gibibytes :: (bytes: u64) -> float64 {
    return bytes_to_file_size(bytes, .GIGA, .BINARY);
}

bytes_to_tebibytes :: (bytes: u64) -> float64 {
    return bytes_to_file_size(bytes, .TERA, .BINARY);
}

kilobytes_to_bytes :: (kilobytes: u64) -> u64 {
    return kilobytes * file_size_unit_multiplier(.KILO, .DECIMAL);
}

megabytes_to_bytes :: (megabytes: u64) -> u64 {
    return megabytes * file_size_unit_multiplier(.MEGA, .DECIMAL);
}

gigabytes_to_bytes :: (gigabytes: u64) -> u64 {
    return gigabytes * file_size_unit_multiplier(.GIGA, .DECIMAL);
}

terabytes_to_bytes :: (terabytes: u64) -> u64 {
    return terabytes * file_size_unit_multiplier(.TERA, .DECIMAL);
}

kibibytes_to_bytes :: (kibibytes: u64) -> u64 {
    return kibibytes * file_size_unit_multiplier(.KILO, .BINARY);
}

mebibytes_to_bytes :: (mebibytes: u64) -> u64 {
    return mebibytes * file_size_unit_multiplier(.MEGA, .BINARY);
}

gibibytes_to_bytes :: (gibibytes: u64) -> u64 {
    return gibibytes * file_size_unit_multiplier(.GIGA, .BINARY);
}

tebibytes_to_bytes :: (tebibytes: u64) -> u64 {
    return tebibytes * file_size_unit_multiplier(.TERA, .BINARY);
}


// just takes bytes and returns a readable byte size, eg 1 000 000 becomes 1MB
readable_byte_size :: (bytes: u64, unit_system := File_Size_Unit_System.BINARY) -> string {
    base := ifx unit_system == .BINARY then cast(u64) 1024 else cast(u64) 1000;

    unit_index := 0;
    divisor: u64 = 1;

    remaining := bytes;
    while remaining >= base && unit_index < 6 {
        remaining /= base;
        divisor *= base;
        unit_index += 1;
    }

    suffix := readable_byte_size_suffix(unit_index, unit_system);
    if unit_index == 0 return tprint("% %", bytes, suffix);

    whole := bytes / divisor;
    remainder := bytes % divisor;
    tenth := (remainder * 10 + divisor / 2) / divisor;

    if tenth == 10 {
        whole += 1;
        tenth = 0;
    }

    if tenth == 0 return tprint("% %", whole, suffix);
    return tprint("% %", tprint("%.%", whole, tenth), suffix);
}

readable_byte_size_suffix :: (unit_index: int, unit_system: File_Size_Unit_System) -> string {
    if unit_system == .BINARY {
        if unit_index == 0 return "B";
        if unit_index == 1 return "KiB";
        if unit_index == 2 return "MiB";
        if unit_index == 3 return "GiB";
        if unit_index == 4 return "TiB";
        if unit_index == 5 return "PiB";
        return "EiB";
    }

    if unit_index == 0 return "B";
    if unit_index == 1 return "KB";
    if unit_index == 2 return "MB";
    if unit_index == 3 return "GB";
    if unit_index == 4 return "TB";
    if unit_index == 5 return "PB";
    return "EB";
}
