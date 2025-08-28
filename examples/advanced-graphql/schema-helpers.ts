import type { GraphQLResolveInfo } from 'graphql'
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

export interface ConnectionConfig {
	findManyOptions: {
		take: number
		where?: any
		orderBy?: any
		include?: any
		cursor?: any
		skip?: number
	}
	countOptions: {
		where?: any
	}
	paginationInfo: {
		first?: number
		last?: number
		after?: string
		before?: string
		cursorField: string
		hasIdField: boolean
		relationFields: string[]
	}
}

export class ConnectionBuilder {
	static buildConfig(args: {
		pagination: PaginationArgs
		where?: any
		orderBy?: any
		include?: any
		info?: any
		relationFields?: string[]
		cursorField?: string
		hasIdField?: boolean
	}): ConnectionConfig {
		const { 
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

		let take = first || last || 10
		if (last) take = -take

		const cursor = (hasIdField && (after || before)) 
			? { [cursorField]: (after || before)! } 
			: undefined
		const skip = cursor ? 1 : 0

		const finalInclude = info ? buildPrismaInclude(info, relationFields) : include

		const findManyOptions: any = {
			take: Math.abs(take) + 1, // Get one extra to check for next page
			where,
			orderBy,
			include: finalInclude,
		}

		if (hasIdField && cursor) {
			findManyOptions.cursor = cursor
			findManyOptions.skip = skip
		}

		return {
			findManyOptions,
			countOptions: { where },
			paginationInfo: {
				first,
				last,
				after,
				before,
				cursorField,
				hasIdField,
				relationFields
			}
		}
	}

	static processResults<T>(
		items: T[],
		totalCount: number,
		paginationInfo: ConnectionConfig['paginationInfo']
	): ConnectionResult<T> {
		const { first, last, cursorField, hasIdField } = paginationInfo

		const hasNextPage = first ? items.length > first : false
		const hasPreviousPage = last ? items.length > Math.abs(last) : false

		const resultItems = hasNextPage || hasPreviousPage ? items.slice(0, -1) : items

		const edges = resultItems.map((item: any, index: number) => {
			let cursor: string
			if (hasIdField && item[cursorField]) {
				cursor = item[cursorField]
			} else {
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


	
	static buildProductConnectionConfig(
		args: ProductQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = 'filter' in args ? FilterBuilder.buildProductFilter((args as any).filter) : {}
		const orderBy = 'sort' in args ? SortBuilder.buildProductSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildProductInclude(info) : PRODUCT_INCLUDES
		
		return this.buildConfig({
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

	static buildReviewConnectionConfig(
		args: ReviewQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = 'filter' in args ? FilterBuilder.buildReviewFilter((args as any).filter) : {}
		const orderBy = 'sort' in args ? SortBuilder.buildReviewSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildReviewInclude(info) : REVIEW_INCLUDES
		
		return this.buildConfig({
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

	static buildTagConnectionConfig(
		args: TagQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = 'filter' in args ? FilterBuilder.buildTagFilter((args as any).filter) : {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildTagInclude(info) : TAG_INCLUDES
		
		return this.buildConfig({
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

	static buildProductTagConnectionConfig(
		args: ProductTagQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildProductTagInclude(info) : PRODUCTTAG_INCLUDES
		
		return this.buildConfig({
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
	static buildFilter(filter: any): any {
		if (!filter || typeof filter !== 'object') return {}

		const where: any = {}

		for (const [field, value] of Object.entries(filter)) {
			if (field === 'AND' && Array.isArray(value)) {
				where.AND = value.map((f: any) => this.buildFilter(f))
			} else if (field === 'OR' && Array.isArray(value)) {
				where.OR = value.map((f: any) => this.buildFilter(f))
			} else if (value && typeof value === 'object') {
				const fieldWhere: any = {}
				
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
	static buildSort(sort: any, fallbackSort: any = { id: 'asc' }): any {
		if (!sort || typeof sort !== 'object') return fallbackSort

		const orderBy: any = {}

		for (const [field, direction] of Object.entries(sort)) {
			if (direction && typeof direction === 'string') {
					orderBy[field] = direction.toLowerCase()
			}
		}

		return Object.keys(orderBy).length > 0 ? orderBy : fallbackSort
	}

	
	static buildProductSort(sort?: ProductSortInput): any {
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}

	static buildReviewSort(sort?: ReviewSortInput): any {
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}


}



export interface ResolveTree {
	name: string
	alias: string
	args: { [str: string]: unknown }
	fieldsByTypeName: FieldsByTypeName
}

export interface FieldsByTypeName {
	[str: string]: { [str: string]: ResolveTree }
}

export function buildPrismaInclude(_resolveInfo: any, relations: string[] = []): any {
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