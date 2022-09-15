package codegen

// if operand is
// 	Temp     -> alloc in stack
//	Register -> use register
//	Local    -> use load/store
//	Mem      -> use load/store
//
// Assume RDX and RAX are never used as general purpose
