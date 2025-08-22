export interface PaginationArgs {
	first: number | null
	after: string | null
	last: number | null
	before: string | null
}

export interface ConnectionResult<T> {
	pageInfo: {
		hasNextPage: boolean
		hasPreviousPage: boolean
		startCursor?: string
		endCursor?: string
	}
	edges: Array<{
		node: T
		cursor: string
	}>
	totalCount: number
}
