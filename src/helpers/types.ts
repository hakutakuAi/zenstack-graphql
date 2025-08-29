import type { GraphQLResolveInfo } from 'graphql'

export interface PaginationArgs {
	first?: number
	after?: string
	last?: number
	before?: string
}

export interface ConnectionConfig<T> {
	findMany: {
		take?: number
		skip?: number
		cursor?: any
		where?: any
		orderBy?: any
		select?: any
	}
	count: {
		where?: any
	}
	toConnection(items: any[], totalCount: number): Connection<T>
}

export interface Connection<T> {
	edges: Array<{ node: T; cursor: string }>
	pageInfo: {
		hasNextPage: boolean
		hasPreviousPage: boolean
		startCursor?: string
		endCursor?: string
	}
	totalCount: number
}

export interface ConnectionArgs<TSelect = any> {
	first?: number
	after?: string
	last?: number
	before?: string
	where?: any
	orderBy?: any
	select?: TSelect
	info?: GraphQLResolveInfo
	cursorField?: string
}

export interface FilterInput {
	[key: string]: any
}

export interface SortInput {
	[key: string]: any
}
