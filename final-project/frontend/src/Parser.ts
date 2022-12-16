
// For example, you expect to parse a given value with `MyType` shape

// // Validate this value with a custom type guard (extend to your needs)
// function isMyType(o: any): o is MyType {
//     return "name" in o && "description" in o
// }

export const safeJsonParse = <T>(guard: (o: any) => o is T) =>
    (text: string): ParseResult<T> => {
        try {
            const parsed = JSON.parse(text)
            return guard(parsed) ? { parsed, hasError: false } : { hasError: true }
        }
        catch (e) {
            return { hasError: true, error: e }
        }
    }

export type ParseResult<T> =
    | { parsed: T; hasError: false; error?: undefined }
    | { parsed?: undefined; hasError: true; error?: unknown }


export function isType2dArray(o: any): o is string[][] {
    return o
}

export function isTypeArray(o: any): o is string[] {
    return o
}

export const safeParse2dArray = safeJsonParse(isType2dArray)
export const safeParseArray = safeJsonParse(isTypeArray)