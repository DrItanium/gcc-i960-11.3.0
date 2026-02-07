# i960 GCC Backend Modernization Session Summary

**Date:** 2026-02-06  
**Repository:** DrItanium/gcc-i960-11.3.0  
**Goal:** Modernize i960 backend from GCC 3.4.6 patterns to GCC 11.3.0 standards, with VLA support

---

## Issues Identified and Fixed

### ✅ **1. VARARGS_STDARG_FUNCTION Macro (COMPLETED)**
**Problem:** Used obsolete GCC 3.4.6 implementation  
**Location:** `gcc/config/i960/i960.c` (lines 123-126)  

**Old code:**
```c
#define VARARGS_STDARG_FUNCTION(FNDECL) \
(TYPE_ARG_TYPES (TREE_TYPE (FNDECL)) != 0 \
  && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (TREE_TYPE (FNDECL))))) \
      != void_type_node)
```

**Fixed to:**
```c
#define VARARGS_STDARG_FUNCTION(FNDECL) stdarg_p(TREE_TYPE(FNDECL))
```

---

### ✅ **2. current_function_args_size VLA Crash (COMPLETED)**
**Problem:** `crtl->args.size.to_constant()` crashes when arguments include VLAs  
**Location:** `gcc/config/i960/i960.c` (line 70)

**Solution:** Updated macro to handle variable-sized arguments safely:
```c
static inline HOST_WIDE_INT 
get_current_function_args_size(void) 
{
    if (crtl->args.size.is_constant())
        return crtl->args.size.to_constant();
    
    /* For variable-sized arguments (VLAs), conservatively return
       the arg block size (48 bytes on i960) */
    return 48;
}
#define current_function_args_size get_current_function_args_size()
```

**Impact:** Prevents crashes, ensures correct stack frame allocation with VLAs

---

### ✅ **3. Deprecated Register Macros (COMPLETED)**
**Problem:** `REGNO_OK_FOR_INDEX_P`, `REG_OK_FOR_INDEX_P`, etc. are deprecated in GCC 11  
**Location:** `gcc/config/i960/i960.h` (lines 533-589)

**Action:** Removed these macros, functionality handled by `TARGET_LEGITIMATE_ADDRESS_P` hook

---

### ✅ **4. VLA Crashes in .md File Patterns (COMPLETED)**
**Problem:** Multiple move patterns called `crtl->args.size.to_constant()` directly in conditions, causing crashes with VLAs  
**Location:** `gcc/config/i960/i960.md` (multiple patterns)

**Patterns affected:**
- `*movsi2_v0` / `*movsi2_v1`
- `*movqi2_v1`
- `*movdi2_v0` / `*movdi2_v1`
- Similar patterns for other modes

**Solution:** Created helper functions and updated all patterns:

```c
/* In i960.c */
bool
i960_can_use_g14_for_zero_store (void)
{
  if (!crtl->args.size.is_constant ())
    return false;
  
  return !currently_expanding_to_rtl
         && crtl->args.size.to_constant () == 0
         && !cfun->stdarg;
}

bool
i960_cannot_use_g14_for_zero_store (void)
{
  if (!crtl->args.size.is_constant ())
    return true;
  
  return currently_expanding_to_rtl
         || crtl->args.size.to_constant () != 0
         || cfun->stdarg;
}
```

**Updated pattern conditions:**
```markdown
/* Before */
"(( crtl->args.size.to_constant() ) == 0
    && (cfun->stdarg) == 0
    && currently_expanding_to_rtl == 0)"

/* After */
"i960_can_use_g14_for_zero_store ()"
```

---

### ✅ **5. CONSTANT_ADDRESS_P Macro (COMPLETED)**
**Problem:** Deprecated macro in modern GCC  
**Location:** `gcc/config/i960/i960.h` (line 551-553)

**Action:** Removed macro entirely (functionality handled by `TARGET_LEGITIMATE_ADDRESS_P`)

---

## Remaining Modernization Tasks

### 🔴 **Priority #4: Remove Deprecated Debug Macros**
**Location:** `gcc/config/i960/i960.h` (lines 119-126)

**Lines to delete:**
```c
/* Generate DBX debugging information.  */
#define DBX_DEBUGGING_INFO 1

/* Generate DBX_DEBUGGING_INFO by default.  */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Redefine this to print in hex.  No value adjustment is necessary
   anymore.  */
#define PUT_SDB_TYPE(A) \
  fprintf (asm_out_file, "\t.type\t0x%x;", A)
```

**Reason:** 
- `PREFERRED_DEBUGGING_TYPE` is deprecated in GCC 11 (DWARF is now default)
- `PUT_SDB_TYPE` is for ancient SDB debugging format
- GCC 11 uses DWARF debugging by default

---

### 🟡 **Priority #6: Clean Up or Delete i960_initialize()**
**Location:** `gcc/config/i960/i960.c` (lines 148-188)

**Current state:** Function is mostly commented-out code with only 2 lines of actual code

**Recommended action:** Move initialization to `i960_option_override()` and delete function:

```c
void
i960_option_override (void)
{
  /* Set the per-function-data initializer.  */
  init_machine_status = i960_init_machine_status;
  
  /* Initialize bit alignment settings for pragmas */
  i960_maxbitalignment = 128;
  i960_last_maxbitalignment = 8;
}
```

**Then:**
1. Delete `i960_initialize()` from `gcc/config/i960/i960.c`
2. Delete declaration from `gcc/config/i960/i960-protos.h` (line 82)
3. Search for and remove any calls to `i960_initialize()`

**Alternative:** If function is called from somewhere you can't easily change, simplify it by removing all `#if 0` blocks

---

### 🔵 **Priority #7: Update FSF Copyright Addresses**
**Reason:** FSF moved in 2005, addresses are outdated

**Files to update:**
- `gcc/config/i960/i960.c`
- `gcc/config/i960/i960.h`
- `gcc/config/i960/i960-protos.h`
- `gcc/config/i960/i960.md`
- `gcc/config/i960/predicates.md`
- Any other i960 source files

**Global search and replace:**

**Find:**
```
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.
```

**Replace with:**
```
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.
```

---

### 🟢 **Priority #8: Check for Unused Macros**

**Macros to verify:**

1. **`REGNO_OK_FOR_FP_P`** (i960.h line 537)
   ```bash
   grep -r "REGNO_OK_FOR_FP_P" gcc/config/i960/
   ```
   - If only defined, never used → Delete it

2. **`FP_REG_P`** (i960.h line 548)
   ```bash
   grep -r "FP_REG_P" gcc/config/i960/
   ```
   - If only used in `.md` file → OK to keep
   - If unused → Delete it

---

## Technical Details

### Understanding the g14 (Argument Pointer) Optimization

The i960 calling convention defines register `g14` as the argument pointer. When a function:
- Has no arguments (`args.size == 0`)
- Is not varargs/stdarg
- Is past RTL expansion

Then `g14` contains zero and can be used to store zero to memory without loading a constant.

**The "v0" patterns:** Used when `g14` can be used for zero stores  
**The "v1" patterns:** Used when `g14` cannot be used (have args, varargs, etc.)

---

## Key GCC 11 Modernizations Applied

1. **`poly_int` handling:** Check `is_constant()` before calling `to_constant()`
2. **Modern API usage:** `stdarg_p()` instead of manual AST traversal
3. **Target hooks over macros:** Use `TARGET_*` hooks instead of deprecated macros
4. **Conservative VLA handling:** Assume need for arg block space when size is variable

---

## Files Modified

- ✅ `gcc/config/i960/i960.c` - Core backend implementation
- ✅ `gcc/config/i960/i960.h` - Target header definitions
- ✅ `gcc/config/i960/i960-protos.h` - Function prototypes
- ✅ `gcc/config/i960/i960.md` - Machine description patterns

---

## Testing Recommendations

After completing remaining tasks, test:
1. **VLA support:** Compile functions with VLA arguments
2. **Varargs:** Test functions with `...` arguments
3. **Zero stores:** Verify `g14` optimization still works
4. **Debugging:** Ensure DWARF debugging info is generated correctly
5. **All architectures:** Test all `-m*` variants (ka, kb, sa, sb, mc, ca, cc, cf, ja, jd, jf, rp)

---

## References

- GCC 11 Porting Guide: https://gcc.gnu.org/gcc-11/porting_to.html
- `poly_int` documentation in GCC source: `gcc/poly-int.h`
- Target macro to hook conversion: `gcc/target.def`

---

## Session Notes

- Successfully identified and fixed critical VLA crash bugs
- Removed deprecated GCC 3.4.6 patterns
- Created VLA-safe helper functions for move patterns
- Simplified code by using modern GCC APIs
- Remaining tasks are mostly cleanup (removing dead code, updating headers)

**Estimated time to complete remaining tasks:** ~30 minutes


-----

Looks like the UNGE and other operations are not automatically reversible and
require a separate call to work right. This will need me to update the
print_operand function to call the proper function. This is specifically a
floating point issue.
