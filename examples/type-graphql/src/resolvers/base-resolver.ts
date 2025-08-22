import { PrismaClient } from '@prisma/client'
import { ConnectionBuilder, FilterBuilder, SortBuilder } from '../utils/resolver-helpers'
import { PaginationArgs, ConnectionResult } from '../utils/types'

export abstract class BaseResolver {
	protected async findMany<T>(
		prisma: PrismaClient,
		model: string,
		options: {
			where?: any
			orderBy?: any
			include?: any
		} = {},
	): Promise<T[]> {
		const modelDelegate = (prisma as any)[model]
		return (await modelDelegate.findMany({
			...options,
		})) as T[]
	}

	protected async findUnique<T>(prisma: PrismaClient, model: string, where: any, include?: any): Promise<T | null> {
		const modelDelegate = (prisma as any)[model]
		return (await modelDelegate.findUnique({
			where,
			include,
		})) as T | null
	}

	protected async create<T>(prisma: PrismaClient, model: string, data: any, include?: any): Promise<T> {
		const modelDelegate = (prisma as any)[model]
		return (await modelDelegate.create({
			data,
			include,
		})) as T
	}

	protected async update<T>(prisma: PrismaClient, model: string, where: any, data: any, include?: any): Promise<T | null> {
		const modelDelegate = (prisma as any)[model]
		return (await modelDelegate.update({
			where,
			data,
			include,
		})) as T | null
	}

	protected async buildConnection<T extends { id: string }>(
		prisma: PrismaClient,
		model: string,
		pagination: PaginationArgs,
		options: {
			where?: any
			orderBy?: any
			include?: any
		} = {},
	): Promise<ConnectionResult<T>> {
		return ConnectionBuilder.build<T>({
			prisma,
			model,
			pagination,
			...options,
		})
	}

	protected async buildRelayConnection<T extends { id: string }>(
		prisma: PrismaClient,
		model: string,
		args: {
			filter?: any
			sort?: any
			first?: number | null
			after?: string | null
			last?: number | null
			before?: string | null
		},
		allowedFilterFields: string[],
		allowedSortFields: string[],
		include?: any,
	): Promise<ConnectionResult<T>> {
		const { filter, sort, first, after, last, before } = args

		const pagination: PaginationArgs = {
			first: first ?? (last ? null : 10),
			after: after ?? null,
			last: last ?? null,
			before: before ?? null,
		}

		const where = FilterBuilder.buildGeneric(filter, allowedFilterFields)
		const orderBy = SortBuilder.buildGeneric(sort, allowedSortFields)

		return this.buildConnection<T>(prisma, model, pagination, {
			where,
			orderBy,
			include,
		})
	}

	protected async buildCompositeKeyConnection<T>(
		prisma: PrismaClient,
		model: string,
		args: {
			filter?: any
			sort?: any
			first?: number | null
			after?: string | null
			last?: number | null
			before?: string | null
		},
		allowedFilterFields: string[],
		allowedSortFields: string[],
		getCursor: (item: T) => string,
		include?: any,
	): Promise<ConnectionResult<T>> {
		const { filter, sort, first, after, last, before } = args

		const pagination: PaginationArgs = {
			first: first ?? (last ? null : 10),
			after: after ?? null,
			last: last ?? null,
			before: before ?? null,
		}

		const where = FilterBuilder.buildGeneric(filter, allowedFilterFields)
		const orderBy = SortBuilder.buildGeneric(sort, allowedSortFields)

		const modelDelegate = (prisma as any)[model]

		let take = pagination.first || pagination.last || 10
		if (pagination.last) take = -take

		const items = await modelDelegate.findMany({
			take,
			where,
			orderBy: orderBy || { assignedAt: 'asc' },
			include,
		})

		const totalCount = await modelDelegate.count({ where })

		const edges = items.map((item: T) => ({
			node: item,
			cursor: getCursor(item),
		}))

		const hasNextPage = pagination.first ? items.length === pagination.first : false
		const hasPreviousPage = pagination.last ? items.length === Math.abs(pagination.last) : false

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
