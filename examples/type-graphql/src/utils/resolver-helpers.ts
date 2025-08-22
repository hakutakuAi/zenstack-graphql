import { PrismaClient } from '@prisma/client'
import { PaginationArgs, ConnectionResult } from './types'

export const PRISMA_INCLUDES = {
	POST: {
		author: true,
		categories: true,
		comments: true,
	},
	COMMENT: {
		post: true,
		author: true,
	},
	USER: {},
	CATEGORY: {},
	POST_CATEGORY: {},
} as const

export class ConnectionBuilder {
	static async build<T extends { id: string }>(findManyArgs: {
		prisma: PrismaClient
		model: string
		pagination: PaginationArgs
		where?: any
		orderBy?: any
		include?: any
	}): Promise<ConnectionResult<T>> {
		const { prisma, model, pagination, where, orderBy, include } = findManyArgs
		const { first, after, last, before } = pagination

		let take = first || last || 10
		if (last) take = -take

		const cursor = after || before ? { id: (after || before)! } : undefined
		const skip = cursor ? 1 : 0

		const modelDelegate = (prisma as any)[model]

		const items = await modelDelegate.findMany({
			take,
			skip,
			cursor,
			where,
			orderBy: orderBy || { id: 'asc' },
			include,
		})

		const totalCount = await modelDelegate.count({ where })

		const edges = items.map((item: T) => ({
			node: item,
			cursor: item.id,
		}))

		const hasNextPage = first ? items.length === first : false
		const hasPreviousPage = last ? items.length === Math.abs(last) : false

		return {
			pageInfo: {
				hasNextPage,
				hasPreviousPage,
				startCursor: edges[0]?.cursor,
				endCursor: edges[edges.length - 1]?.cursor,
			},
			edges,
			totalCount,
		}
	}
}

export class FilterBuilder {
	static buildGeneric(filter: any, allowedFields: string[]): any {
		if (!filter) return {}

		const where: any = {}

		for (const field of allowedFields) {
			if (filter[field]) {
				const fieldFilter = filter[field]

				if (typeof fieldFilter === 'object') {
					where[field] = {}
					if (fieldFilter.equals !== undefined) where[field].equals = fieldFilter.equals
					if (fieldFilter.contains) where[field].contains = fieldFilter.contains
					if (fieldFilter.startsWith) where[field].startsWith = fieldFilter.startsWith
					if (fieldFilter.endsWith) where[field].endsWith = fieldFilter.endsWith
					if (fieldFilter.in) where[field].in = fieldFilter.in
					if (fieldFilter.notIn) where[field].notIn = fieldFilter.notIn
					if (fieldFilter.gt) where[field].gt = fieldFilter.gt
					if (fieldFilter.lt) where[field].lt = fieldFilter.lt
				}
			}
		}

		if (filter.AND) where.AND = filter.AND.map((f: any) => FilterBuilder.buildGeneric(f, allowedFields))
		if (filter.OR) where.OR = filter.OR.map((f: any) => FilterBuilder.buildGeneric(f, allowedFields))

		return where
	}
}

export class SortBuilder {
	static buildGeneric(sort: any, allowedFields: string[]): any {
		if (!sort) return {}

		const orderBy: any = {}

		for (const field of allowedFields) {
			if (sort[field]) {
				orderBy[field] = sort[field].toLowerCase()
			}
		}

		return orderBy
	}
}
