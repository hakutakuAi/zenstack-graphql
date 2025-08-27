import type { GraphQLResolveInfo } from 'graphql'
import type { PrismaClient } from '@prisma/client'
import { Product, ProductQueryArgs, ProductConnection, ProductFilterInput, ProductSortInput, Review, ReviewQueryArgs, ReviewConnection, ReviewFilterInput, ReviewSortInput, Tag, TagQueryArgs, TagConnection, TagFilterInput, ProductTag, ProductTagQueryArgs, ProductTagConnection, ProductTagFilterInput } from './schema'

export interface PaginationArgs {
	first?: number
	after?: string
	last?: number
	before?: string
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

export class ConnectionBuilder {
	/**
	 * Generic connection builder that handles both regular models and composite key models
	 */
	static async build<T>(args: {
		prisma: PrismaClient
		model: string
		pagination: PaginationArgs
		where?: any
		orderBy?: any
		include?: any
		info?: GraphQLResolveInfo
		relationFields?: string[]
		cursorField?: string
		hasIdField?: boolean
	}): Promise<ConnectionResult<T>> {
		const { 
			prisma, 
			model, 
			pagination, 
			where, 
			orderBy, 
			include, 
			info, 
			relationFields = [],
			cursorField = 'id',
			hasIdField = true
		} = args
		const { first, after, last, before } = pagination

		// Calculate pagination parameters
		let take = first || last || 10
		if (last) take = -take

		// For composite key models, we skip cursor-based pagination
		const cursor = (hasIdField && (after || before)) 
			? { [cursorField]: (after || before)! } 
			: undefined
		const skip = cursor ? 1 : 0

		// Build include from GraphQL selection if info is provided
		const finalInclude = info ? buildPrismaInclude(info, relationFields) : include

		// Get model delegate
		const modelDelegate = (prisma as any)[model.toLowerCase()]

		// Prepare query options
		const queryOptions: any = {
			take: Math.abs(take) + 1, // Get one extra to check for next page
			where,
			orderBy,
			include: finalInclude,
		}

		// Only add cursor and skip for models with ID field
		if (hasIdField && cursor) {
			queryOptions.cursor = cursor
			queryOptions.skip = skip
		}

		// Fetch items
		const items = await modelDelegate.findMany(queryOptions)

		// Calculate totalCount
		const totalCount = await modelDelegate.count({ where })

		// Determine pagination info
		const hasNextPage = first ? items.length > first : false
		const hasPreviousPage = last ? items.length > Math.abs(last) : false

		// Remove extra item if present
		const resultItems = hasNextPage || hasPreviousPage ? items.slice(0, -1) : items

		// Build edges - use composite key for cursor if no ID field
		const edges = resultItems.map((item: any, index: number) => {
			let cursor: string
			if (hasIdField && item[cursorField]) {
				cursor = item[cursorField]
			} else {
				// For composite key models, create a cursor from available fields or use index
				cursor = item.postId && item.categoryId 
					? `${item.postId}:${item.categoryId}`
					: String(index)
			}
			
			return {
				node: item,
				cursor,
			}
		})

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

	
	static async buildProductConnection(
		prisma: PrismaClient,
		args: ProductQueryArgs,
		info?: GraphQLResolveInfo
	): Promise<ProductConnection> {
		const where = 'filter' in args ? FilterBuilder.buildProductFilter((args as any).filter) : {}
		const orderBy = 'sort' in args ? SortBuilder.buildProductSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildProductInclude(info) : PRODUCT_INCLUDES
		
		return this.build<Product>({
			prisma,
			model: 'product',
			pagination: {
				first: args.first,
				after: args.after,
				last: args.last,
				before: args.before,
			},
			where,
			orderBy,
			include,
			info,
			relationFields: ['reviews', 'tags'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static async buildReviewConnection(
		prisma: PrismaClient,
		args: ReviewQueryArgs,
		info?: GraphQLResolveInfo
	): Promise<ReviewConnection> {
		const where = 'filter' in args ? FilterBuilder.buildReviewFilter((args as any).filter) : {}
		const orderBy = 'sort' in args ? SortBuilder.buildReviewSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildReviewInclude(info) : REVIEW_INCLUDES
		
		return this.build<Review>({
			prisma,
			model: 'review',
			pagination: {
				first: args.first,
				after: args.after,
				last: args.last,
				before: args.before,
			},
			where,
			orderBy,
			include,
			info,
			relationFields: ['product'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static async buildTagConnection(
		prisma: PrismaClient,
		args: TagQueryArgs,
		info?: GraphQLResolveInfo
	): Promise<TagConnection> {
		const where = 'filter' in args ? FilterBuilder.buildTagFilter((args as any).filter) : {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildTagInclude(info) : TAG_INCLUDES
		
		return this.build<Tag>({
			prisma,
			model: 'tag',
			pagination: {
				first: args.first,
				after: args.after,
				last: args.last,
				before: args.before,
			},
			where,
			orderBy,
			include,
			info,
			relationFields: ['products'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static async buildProductTagConnection(
		prisma: PrismaClient,
		args: ProductTagQueryArgs,
		info?: GraphQLResolveInfo
	): Promise<ProductTagConnection> {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildProductTagInclude(info) : PRODUCTTAG_INCLUDES
		
		return this.build<ProductTag>({
			prisma,
			model: 'producttag',
			pagination: {
				first: args.first,
				after: args.after,
				last: args.last,
				before: args.before,
			},
			where,
			orderBy,
			include,
			info,
			relationFields: ['product', 'tag'],
			hasIdField: false,
			cursorField: 'id',
		})
	}
}

export class FilterBuilder {
	/**
	 * Build Prisma where clause from GraphQL filter input dynamically
	 * This approach uses runtime reflection to map filter operations
	 */
	static buildFilter(filter: any): any {
		if (!filter || typeof filter !== 'object') return {}

		const where: any = {}

		for (const [field, value] of Object.entries(filter)) {
			if (field === 'AND' && Array.isArray(value)) {
				where.AND = value.map((f: any) => this.buildFilter(f))
			} else if (field === 'OR' && Array.isArray(value)) {
				where.OR = value.map((f: any) => this.buildFilter(f))
			} else if (value && typeof value === 'object') {
				// Map filter operations dynamically
				const fieldWhere: any = {}
				
				// Copy all valid operations from the filter value
				for (const [operation, operationValue] of Object.entries(value)) {
					if (operationValue !== undefined && operationValue !== null) {
						fieldWhere[operation] = operationValue
					}
				}
				
				if (Object.keys(fieldWhere).length > 0) {
					where[field] = fieldWhere
				}
			}
		}

		return where
	}

	
	static buildProductFilter(filter?: ProductFilterInput): any {
		return this.buildFilter(filter)
	}

	static buildReviewFilter(filter?: ReviewFilterInput): any {
		return this.buildFilter(filter)
	}

	static buildTagFilter(filter?: TagFilterInput): any {
		return this.buildFilter(filter)
	}

}

export class SortBuilder {
	/**
	 * Build Prisma orderBy clause from GraphQL sort input dynamically
	 * This approach uses runtime reflection to map sort fields
	 */
	static buildSort(sort: any, fallbackSort: any = { id: 'asc' }): any {
		if (!sort || typeof sort !== 'object') return fallbackSort

		const orderBy: any = {}

		for (const [field, direction] of Object.entries(sort)) {
			if (direction && typeof direction === 'string') {
				// Convert enum values to lowercase for Prisma
				orderBy[field] = direction.toLowerCase()
			}
		}

		return Object.keys(orderBy).length > 0 ? orderBy : fallbackSort
	}

	
	static buildProductSort(sort?: ProductSortInput): any {
		// For models without id field (like composite key models), don't use id as fallback
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}

	static buildReviewSort(sort?: ReviewSortInput): any {
		// For models without id field (like composite key models), don't use id as fallback
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}


}

// Simplified field selection utilities (GraphQL-import-free)
// Uses static includes instead of dynamic GraphQL field parsing

export interface ResolveTree {
	name: string
	alias: string
	args: { [str: string]: unknown }
	fieldsByTypeName: FieldsByTypeName
}

export interface FieldsByTypeName {
	[str: string]: { [str: string]: ResolveTree }
}

// Simplified version that doesn't require GraphQL imports
export function buildPrismaInclude(_resolveInfo: any, relations: string[] = []): any {
	// For now, return a simple include object based on available relations
	// This avoids GraphQL module conflicts while maintaining basic functionality
	const include: any = {}
	
	relations.forEach(relation => {
		include[relation] = true
	})
	
	return include
}

export class FieldSelection {
	
	static buildProductInclude(info?: any): any {
		const relationFields = ['reviews', 'tags']
		return buildPrismaInclude(info, relationFields)
	}

	static buildReviewInclude(info?: any): any {
		const relationFields = ['product']
		return buildPrismaInclude(info, relationFields)
	}

	static buildTagInclude(info?: any): any {
		const relationFields = ['products']
		return buildPrismaInclude(info, relationFields)
	}

	static buildProductTagInclude(info?: any): any {
		const relationFields = ['product', 'tag']
		return buildPrismaInclude(info, relationFields)
	}
}


export const PRODUCT_INCLUDES = {
	reviews: true,
	tags: true
}

export const REVIEW_INCLUDES = {
	product: true
}

export const TAG_INCLUDES = {
	products: true
}

export const PRODUCTTAG_INCLUDES = {
	product: true,
	tag: true
}