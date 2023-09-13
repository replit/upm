export interface Directory {
	type: 'directory';
	contents: Record<string, File | Directory>;
}

export interface File {
	type: 'file';
	contents: string;
}

export type Result<T, E> = { Ok: T; Err: undefined; } | { Ok: undefined; Err: E; };
export const Ok = <T, E>(value: T): Result<T, E> => ({ Ok: value, Err: undefined });
export const Err = <T, E>(error: E): Result<T, E> => ({ Ok: undefined, Err: error });
