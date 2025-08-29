import type { ConnectionArgs, ConnectionConfig, Connection, PaginationArgs } from './types'

export function buildConnection<T, TSelect = any>(args: ConnectionArgs<TSelect>): ConnectionConfig<T> {
	const { first, after, last, before, where, orderBy, select, cursorField = 'id' } = args

	const pagination = { first, after, last, before }
	const take = calculateTake(pagination)
	const cursor = buildCursor(after || before, cursorField)
	const skip = cursor ? 1 : 0

	const findMany = {
		take: Math.abs(take) + 1,
		where,
		orderBy,
		select,
		...(cursor && { cursor, skip }),
	}

	const count = { where }

	return {
		findMany,
		count,
		toConnection: (items: any[], totalCount: number) => processResults(items, totalCount, pagination, cursorField),
	}
}

function calculateTake(pagination: PaginationArgs): number {
	const { first, last } = pagination
	let take = first || last || 10
	if (last) take = -take
	return take
}

function buildCursor(cursorValue?: string, cursorField = 'id'): any {
	if (!cursorValue) return undefined

	if (cursorValue.includes(':')) {
		const parts = cursorValue.split(':')
		const field = parts[0] as string
		const value = parts[1]
		return { [field]: value }
	}

	const field = cursorField as string
	return { [field]: cursorValue }
}

function processResults<T>(items: T[], totalCount: number, pagination: PaginationArgs, cursorField: string): Connection<T> {
	const { first, last } = pagination

	const hasNextPage = first ? items.length > first : false
	const hasPreviousPage = last ? items.length > Math.abs(last) : false

	const resultItems = hasNextPage || hasPreviousPage ? items.slice(0, -1) : items

	const edges = resultItems.map((item: any) => ({
		node: item,
		cursor: extractCursor(item, cursorField),
	}))

	return {
		edges,
		pageInfo: {
			hasNextPage,
			hasPreviousPage,
			startCursor: edges[0]?.cursor,
			endCursor: edges[edges.length - 1]?.cursor,
		},
		totalCount,
	}
}

function extractCursor(item: any, cursorField: string): string {
	const value = item[cursorField]
	return value ? String(value) : String(Math.random())
}
