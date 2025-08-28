import type { GraphQLResolveInfo } from 'graphql'
import { Customer, CustomerQueryArgs, CustomerConnection, CustomerFilterInput, Product, ProductQueryArgs, ProductConnection, ProductFilterInput, Address, AddressQueryArgs, AddressConnection, AddressFilterInput, Order, OrderQueryArgs, OrderConnection, OrderFilterInput, OrderItem, OrderItemQueryArgs, OrderItemConnection, OrderItemFilterInput, Category, CategoryQueryArgs, CategoryConnection, CategoryFilterInput } from './schema'

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


	
	static buildCustomerConnectionConfig(
		args: CustomerQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildCustomerInclude(info) : CUSTOMER_INCLUDES
		
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
			relationFields: ['shippingAddress', 'billingAddress', 'orders'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildProductConnectionConfig(
		args: ProductQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
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
			relationFields: ['categories', 'orderItems'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildAddressConnectionConfig(
		args: AddressQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildAddressInclude(info) : ADDRESS_INCLUDES
		
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
			relationFields: ['customerShipping', 'customerBilling'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildOrderConnectionConfig(
		args: OrderQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildOrderInclude(info) : ORDER_INCLUDES
		
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
			relationFields: ['customer', 'orderItems'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildOrderItemConnectionConfig(
		args: OrderItemQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildOrderItemInclude(info) : ORDERITEM_INCLUDES
		
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
			relationFields: ['order', 'product'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildCategoryConnectionConfig(
		args: CategoryQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildCategoryInclude(info) : CATEGORY_INCLUDES
		
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
			relationFields: ['products', 'parentCategory', 'subCategories'],
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
	
	static buildCustomerInclude(info?: any): any {
		const relationFields = ['shippingAddress', 'billingAddress', 'orders']
		return buildPrismaInclude(info, relationFields)
	}

	static buildProductInclude(info?: any): any {
		const relationFields = ['categories', 'orderItems']
		return buildPrismaInclude(info, relationFields)
	}

	static buildAddressInclude(info?: any): any {
		const relationFields = ['customerShipping', 'customerBilling']
		return buildPrismaInclude(info, relationFields)
	}

	static buildOrderInclude(info?: any): any {
		const relationFields = ['customer', 'orderItems']
		return buildPrismaInclude(info, relationFields)
	}

	static buildOrderItemInclude(info?: any): any {
		const relationFields = ['order', 'product']
		return buildPrismaInclude(info, relationFields)
	}

	static buildCategoryInclude(info?: any): any {
		const relationFields = ['products', 'parentCategory', 'subCategories']
		return buildPrismaInclude(info, relationFields)
	}
}


export const CUSTOMER_INCLUDES = {
	shippingAddress: true,
	billingAddress: true,
	orders: true
}

export const PRODUCT_INCLUDES = {
	categories: true,
	orderItems: true
}

export const ADDRESS_INCLUDES = {
	customerShipping: true,
	customerBilling: true
}

export const ORDER_INCLUDES = {
	customer: true,
	orderItems: true
}

export const ORDERITEM_INCLUDES = {
	order: true,
	product: true
}

export const CATEGORY_INCLUDES = {
	products: true,
	parentCategory: true,
	subCategories: true
}