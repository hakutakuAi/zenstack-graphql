import type { GraphQLResolveInfo } from 'graphql'
import { Author, AuthorQueryArgs, AuthorConnection, AuthorFilterInput, Book, BookQueryArgs, BookConnection, BookFilterInput, Review, ReviewQueryArgs, ReviewConnection, ReviewFilterInput, Article, ArticleQueryArgs, ArticleConnection, ArticleFilterInput, Publisher, PublisherQueryArgs, PublisherConnection, PublisherFilterInput } from './schema'

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
	/**
	 * Build connection configuration without executing queries
	 */
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

		// Prepare query options
		const findManyOptions: any = {
			take: Math.abs(take) + 1, // Get one extra to check for next page
			where,
			orderBy,
			include: finalInclude,
		}

		// Only add cursor and skip for models with ID field
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

	/**
	 * Process query results into connection format
	 */
	static processResults<T>(
		items: T[],
		totalCount: number,
		paginationInfo: ConnectionConfig['paginationInfo']
	): ConnectionResult<T> {
		const { first, last, cursorField, hasIdField } = paginationInfo

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


	
	static buildAuthorConnectionConfig(
		args: AuthorQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildAuthorInclude(info) : AUTHOR_INCLUDES
		
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
			relationFields: ['books', 'articles'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildBookConnectionConfig(
		args: BookQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildBookInclude(info) : BOOK_INCLUDES
		
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
			relationFields: ['author', 'reviews'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildReviewConnectionConfig(
		args: ReviewQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
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
			relationFields: ['book'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildArticleConnectionConfig(
		args: ArticleQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildArticleInclude(info) : ARTICLE_INCLUDES
		
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
			relationFields: ['author'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildPublisherConnectionConfig(
		args: PublisherQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildPublisherInclude(info) : PUBLISHER_INCLUDES
		
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
			relationFields: [],
			hasIdField: true,
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
	
	static buildAuthorInclude(info?: any): any {
		const relationFields = ['books', 'articles']
		return buildPrismaInclude(info, relationFields)
	}

	static buildBookInclude(info?: any): any {
		const relationFields = ['author', 'reviews']
		return buildPrismaInclude(info, relationFields)
	}

	static buildReviewInclude(info?: any): any {
		const relationFields = ['book']
		return buildPrismaInclude(info, relationFields)
	}

	static buildArticleInclude(info?: any): any {
		const relationFields = ['author']
		return buildPrismaInclude(info, relationFields)
	}

	static buildPublisherInclude(info?: any): any {
		const relationFields = []
		return buildPrismaInclude(info, relationFields)
	}
}


export const AUTHOR_INCLUDES = {
	books: true,
	articles: true
}

export const BOOK_INCLUDES = {
	author: true,
	reviews: true
}

export const REVIEW_INCLUDES = {
	book: true
}

export const ARTICLE_INCLUDES = {
	author: true
}

export const PUBLISHER_INCLUDES = {
	
}