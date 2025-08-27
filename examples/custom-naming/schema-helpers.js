// Generated helper utilities for GraphQL resolvers
const { buildPrismaInclude } = require('./graphql-field-selection');

import type { GraphQLResolveInfo } from 'graphql'
import type { PrismaClient } from '@prisma/client'
import { buildPrismaInclude } from './graphql-field-selection'
import { Customer, CustomerQueryArgs, CustomerConnection, CustomerFilterInput, CustomerSortInput, ProductItem, ProductItemQueryArgs, ProductItemConnection, ProductItemFilterInput, ProductItemSortInput, Address, AddressQueryArgs, AddressConnection, AddressFilterInput, AddressSortInput, Order, OrderQueryArgs, OrderConnection, OrderFilterInput, OrderSortInput, OrderItem, OrderItemQueryArgs, OrderItemConnection, OrderItemFilterInput, OrderItemSortInput, Category, CategoryQueryArgs, CategoryConnection, CategoryFilterInput, CategorySortInput } from './schema'

 from './graphql-field-selection'

 = args
		const { first, after, last, before } = pagination

		// Calculate pagination parameters
		let take = first || last || 10
		if (last) take = -take

		const cursor = after || before ? { id)! } : undefined
		const skip = cursor ? 1 : 0

		// Build include from GraphQL selection if info is provided
		const finalInclude = info ? buildPrismaInclude(info, relationFields) : include

		// Get model delegate
		const modelDelegate = (prisma as any)[model.toLowerCase()]

		// Fetch items
		const items = await modelDelegate.findMany({
			take) + 1, // Get one extra to check for next page
			skip,
			cursor,
			where,
			orderBy,
			include,
		})

		// Calculate totalCount
		const totalCount = await modelDelegate.count({ where })

		// Determine pagination info
		const hasNextPage = first ? items.length > first : false
		const hasPreviousPage = last ? items.length > Math.abs(last) : false

		// Remove extra item if present
		const resultItems = hasNextPage ? items.slice(0, -1) : items

		// Build edges
		const edges = resultItems.map((item) => ({
			node,
			cursor,
		}))

		return {
			pageInfo,
				hasPreviousPage,
				startCursor,
				endCursor,
			},
			edges,
			totalCount,
		}
	}

	
	static async buildCustomerConnection(
		prisma,
		args,
		info?){
		const where = FilterBuilder.buildCustomerFilter(args.filter)
		const orderBy = SortBuilder.buildCustomerSort(args.sort)
		const include = info ? FieldSelection.buildCustomerInclude(info) ,
			model,
			pagination,
				after,
				last,
				before,
			},
			where,
			orderBy,
			include,
			info,
			relationFields, 'billingAddress', 'orders'],
		})
	}

	static async buildProductItemConnection(
		prisma,
		args,
		info?){
		const where = FilterBuilder.buildProductItemFilter(args.filter)
		const orderBy = SortBuilder.buildProductItemSort(args.sort)
		const include = info ? FieldSelection.buildProductItemInclude(info) ,
			model,
			pagination,
				after,
				last,
				before,
			},
			where,
			orderBy,
			include,
			info,
			relationFields, 'orderItems'],
		})
	}

	static async buildAddressConnection(
		prisma,
		args,
		info?){
		const where = FilterBuilder.buildAddressFilter(args.filter)
		const orderBy = SortBuilder.buildAddressSort(args.sort)
		const include = info ? FieldSelection.buildAddressInclude(info) ,
			model,
			pagination,
				after,
				last,
				before,
			},
			where,
			orderBy,
			include,
			info,
			relationFields, 'customerBilling'],
		})
	}

	static async buildOrderConnection(
		prisma,
		args,
		info?){
		const where = FilterBuilder.buildOrderFilter(args.filter)
		const orderBy = SortBuilder.buildOrderSort(args.sort)
		const include = info ? FieldSelection.buildOrderInclude(info) ,
			model,
			pagination,
				after,
				last,
				before,
			},
			where,
			orderBy,
			include,
			info,
			relationFields, 'orderItems'],
		})
	}

	static async buildOrderItemConnection(
		prisma,
		args,
		info?){
		const where = FilterBuilder.buildOrderItemFilter(args.filter)
		const orderBy = SortBuilder.buildOrderItemSort(args.sort)
		const include = info ? FieldSelection.buildOrderItemInclude(info) ,
			model,
			pagination,
				after,
				last,
				before,
			},
			where,
			orderBy,
			include,
			info,
			relationFields, 'product'],
		})
	}

	static async buildCategoryConnection(
		prisma,
		args,
		info?){
		const where = FilterBuilder.buildCategoryFilter(args.filter)
		const orderBy = SortBuilder.buildCategorySort(args.sort)
		const include = info ? FieldSelection.buildCategoryInclude(info) ,
			model,
			pagination,
				after,
				last,
				before,
			},
			where,
			orderBy,
			include,
			info,
			relationFields, 'parentCategory', 'subCategories'],
		})
	}
}

export class FilterBuilder {
	static buildGeneric(filter, allowedFields)) return {}

		const where: any = {}

		for (const field of allowedFields) {
			if (filter[field]) {
				const fieldFilter = filter[field]

				if (typeof fieldFilter === 'object') {
					where[field] = {}
					if (fieldFilter.equals !== undefined) where[field].equals = fieldFilter.equals
					if (fieldFilter.not !== undefined) where[field].not = fieldFilter.not
					if (fieldFilter.contains) where[field].contains = fieldFilter.contains
					if (fieldFilter.startsWith) where[field].startsWith = fieldFilter.startsWith
					if (fieldFilter.endsWith) where[field].endsWith = fieldFilter.endsWith
					if (fieldFilter.in) where[field].in = fieldFilter.in
					if (fieldFilter.notIn) where[field].notIn = fieldFilter.notIn
					if (fieldFilter.gt !== undefined) where[field].gt = fieldFilter.gt
					if (fieldFilter.gte !== undefined) where[field].gte = fieldFilter.gte
					if (fieldFilter.lt !== undefined) where[field].lt = fieldFilter.lt
					if (fieldFilter.lte !== undefined) where[field].lte = fieldFilter.lte
				}
			}
		}

		if (filter.AND) {
			where.AND = filter.AND.map((f) => FilterBuilder.buildGeneric(f, allowedFields))
		}
		if (filter.OR) {
			where.OR = filter.OR.map((f) => FilterBuilder.buildGeneric(f, allowedFields))
		}

		return where
	}

	
	static buildCustomerFilter(filter?)) return {}

		const where: any = {}

		
		if (filter.id) {
			const fieldFilter = filter.id
			where.id = {}
			
			if (fieldFilter.equals !== undefined) where.id.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.id.not = fieldFilter.not
			if (fieldFilter.contains) where.id.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.id.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.id.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.id.in = fieldFilter.in
			if (fieldFilter.notIn) where.id.notIn = fieldFilter.notIn
		}

		if (filter.firstName) {
			const fieldFilter = filter.firstName
			where.firstName = {}
			
			if (fieldFilter.equals !== undefined) where.firstName.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.firstName.not = fieldFilter.not
			if (fieldFilter.contains) where.firstName.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.firstName.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.firstName.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.firstName.in = fieldFilter.in
			if (fieldFilter.notIn) where.firstName.notIn = fieldFilter.notIn
		}

		if (filter.lastName) {
			const fieldFilter = filter.lastName
			where.lastName = {}
			
			if (fieldFilter.equals !== undefined) where.lastName.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.lastName.not = fieldFilter.not
			if (fieldFilter.contains) where.lastName.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.lastName.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.lastName.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.lastName.in = fieldFilter.in
			if (fieldFilter.notIn) where.lastName.notIn = fieldFilter.notIn
		}

		if (filter.emailAddress) {
			const fieldFilter = filter.emailAddress
			where.emailAddress = {}
			
			if (fieldFilter.equals !== undefined) where.emailAddress.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.emailAddress.not = fieldFilter.not
			if (fieldFilter.contains) where.emailAddress.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.emailAddress.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.emailAddress.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.emailAddress.in = fieldFilter.in
			if (fieldFilter.notIn) where.emailAddress.notIn = fieldFilter.notIn
		}

		if (filter.registrationDate) {
			const fieldFilter = filter.registrationDate
			where.registrationDate = {}
			
			if (fieldFilter.equals !== undefined) where.registrationDate.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.registrationDate.not = fieldFilter.not
			if (fieldFilter.gt !== undefined) where.registrationDate.gt = fieldFilter.gt
			if (fieldFilter.gte !== undefined) where.registrationDate.gte = fieldFilter.gte
			if (fieldFilter.lt !== undefined) where.registrationDate.lt = fieldFilter.lt
			if (fieldFilter.lte !== undefined) where.registrationDate.lte = fieldFilter.lte
			if (fieldFilter.in) where.registrationDate.in = fieldFilter.in
			if (fieldFilter.notIn) where.registrationDate.notIn = fieldFilter.notIn
		}

		if (filter.loyaltyPoints) {
			const fieldFilter = filter.loyaltyPoints
			where.loyaltyPoints = {}
			
			if (fieldFilter.equals !== undefined) where.loyaltyPoints.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.loyaltyPoints.not = fieldFilter.not
			if (fieldFilter.gt !== undefined) where.loyaltyPoints.gt = fieldFilter.gt
			if (fieldFilter.gte !== undefined) where.loyaltyPoints.gte = fieldFilter.gte
			if (fieldFilter.lt !== undefined) where.loyaltyPoints.lt = fieldFilter.lt
			if (fieldFilter.lte !== undefined) where.loyaltyPoints.lte = fieldFilter.lte
			if (fieldFilter.in) where.loyaltyPoints.in = fieldFilter.in
			if (fieldFilter.notIn) where.loyaltyPoints.notIn = fieldFilter.notIn
		}

		if (filter.shippingAddressId) {
			const fieldFilter = filter.shippingAddressId
			where.shippingAddressId = {}
			
			if (fieldFilter.equals !== undefined) where.shippingAddressId.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.shippingAddressId.not = fieldFilter.not
			if (fieldFilter.contains) where.shippingAddressId.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.shippingAddressId.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.shippingAddressId.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.shippingAddressId.in = fieldFilter.in
			if (fieldFilter.notIn) where.shippingAddressId.notIn = fieldFilter.notIn
		}

		if (filter.billingAddressId) {
			const fieldFilter = filter.billingAddressId
			where.billingAddressId = {}
			
			if (fieldFilter.equals !== undefined) where.billingAddressId.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.billingAddressId.not = fieldFilter.not
			if (fieldFilter.contains) where.billingAddressId.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.billingAddressId.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.billingAddressId.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.billingAddressId.in = fieldFilter.in
			if (fieldFilter.notIn) where.billingAddressId.notIn = fieldFilter.notIn
		}

		if (filter.phoneNumber) {
			const fieldFilter = filter.phoneNumber
			where.phoneNumber = {}
			
			if (fieldFilter.equals !== undefined) where.phoneNumber.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.phoneNumber.not = fieldFilter.not
			if (fieldFilter.contains) where.phoneNumber.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.phoneNumber.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.phoneNumber.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.phoneNumber.in = fieldFilter.in
			if (fieldFilter.notIn) where.phoneNumber.notIn = fieldFilter.notIn
		}

		if (filter.AND) {
			where.AND = filter.AND.map((f) => this.buildCustomerFilter(f))
		}
		if (filter.OR) {
			where.OR = filter.OR.map((f) => this.buildCustomerFilter(f))
		}

		return where
	}

	static buildProductItemFilter(filter?)) return {}

		const where: any = {}

		
		if (filter.id) {
			const fieldFilter = filter.id
			where.id = {}
			
			if (fieldFilter.equals !== undefined) where.id.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.id.not = fieldFilter.not
			if (fieldFilter.contains) where.id.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.id.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.id.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.id.in = fieldFilter.in
			if (fieldFilter.notIn) where.id.notIn = fieldFilter.notIn
		}

		if (filter.name) {
			const fieldFilter = filter.name
			where.name = {}
			
			if (fieldFilter.equals !== undefined) where.name.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.name.not = fieldFilter.not
			if (fieldFilter.contains) where.name.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.name.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.name.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.name.in = fieldFilter.in
			if (fieldFilter.notIn) where.name.notIn = fieldFilter.notIn
		}

		if (filter.description) {
			const fieldFilter = filter.description
			where.description = {}
			
			if (fieldFilter.equals !== undefined) where.description.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.description.not = fieldFilter.not
			if (fieldFilter.contains) where.description.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.description.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.description.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.description.in = fieldFilter.in
			if (fieldFilter.notIn) where.description.notIn = fieldFilter.notIn
		}

		if (filter.price) {
			const fieldFilter = filter.price
			where.price = {}
			
			if (fieldFilter.equals !== undefined) where.price.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.price.not = fieldFilter.not
			if (fieldFilter.gt !== undefined) where.price.gt = fieldFilter.gt
			if (fieldFilter.gte !== undefined) where.price.gte = fieldFilter.gte
			if (fieldFilter.lt !== undefined) where.price.lt = fieldFilter.lt
			if (fieldFilter.lte !== undefined) where.price.lte = fieldFilter.lte
			if (fieldFilter.in) where.price.in = fieldFilter.in
			if (fieldFilter.notIn) where.price.notIn = fieldFilter.notIn
		}

		if (filter.stockQuantity) {
			const fieldFilter = filter.stockQuantity
			where.stockQuantity = {}
			
			if (fieldFilter.equals !== undefined) where.stockQuantity.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.stockQuantity.not = fieldFilter.not
			if (fieldFilter.gt !== undefined) where.stockQuantity.gt = fieldFilter.gt
			if (fieldFilter.gte !== undefined) where.stockQuantity.gte = fieldFilter.gte
			if (fieldFilter.lt !== undefined) where.stockQuantity.lt = fieldFilter.lt
			if (fieldFilter.lte !== undefined) where.stockQuantity.lte = fieldFilter.lte
			if (fieldFilter.in) where.stockQuantity.in = fieldFilter.in
			if (fieldFilter.notIn) where.stockQuantity.notIn = fieldFilter.notIn
		}

		if (filter.AND) {
			where.AND = filter.AND.map((f) => this.buildProductItemFilter(f))
		}
		if (filter.OR) {
			where.OR = filter.OR.map((f) => this.buildProductItemFilter(f))
		}

		return where
	}

	static buildAddressFilter(filter?)) return {}

		const where: any = {}

		
		if (filter.id) {
			const fieldFilter = filter.id
			where.id = {}
			
			if (fieldFilter.equals !== undefined) where.id.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.id.not = fieldFilter.not
			if (fieldFilter.contains) where.id.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.id.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.id.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.id.in = fieldFilter.in
			if (fieldFilter.notIn) where.id.notIn = fieldFilter.notIn
		}

		if (filter.streetLine1) {
			const fieldFilter = filter.streetLine1
			where.streetLine1 = {}
			
			if (fieldFilter.equals !== undefined) where.streetLine1.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.streetLine1.not = fieldFilter.not
			if (fieldFilter.contains) where.streetLine1.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.streetLine1.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.streetLine1.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.streetLine1.in = fieldFilter.in
			if (fieldFilter.notIn) where.streetLine1.notIn = fieldFilter.notIn
		}

		if (filter.streetLine2) {
			const fieldFilter = filter.streetLine2
			where.streetLine2 = {}
			
			if (fieldFilter.equals !== undefined) where.streetLine2.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.streetLine2.not = fieldFilter.not
			if (fieldFilter.contains) where.streetLine2.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.streetLine2.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.streetLine2.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.streetLine2.in = fieldFilter.in
			if (fieldFilter.notIn) where.streetLine2.notIn = fieldFilter.notIn
		}

		if (filter.city) {
			const fieldFilter = filter.city
			where.city = {}
			
			if (fieldFilter.equals !== undefined) where.city.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.city.not = fieldFilter.not
			if (fieldFilter.contains) where.city.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.city.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.city.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.city.in = fieldFilter.in
			if (fieldFilter.notIn) where.city.notIn = fieldFilter.notIn
		}

		if (filter.stateProvince) {
			const fieldFilter = filter.stateProvince
			where.stateProvince = {}
			
			if (fieldFilter.equals !== undefined) where.stateProvince.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.stateProvince.not = fieldFilter.not
			if (fieldFilter.contains) where.stateProvince.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.stateProvince.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.stateProvince.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.stateProvince.in = fieldFilter.in
			if (fieldFilter.notIn) where.stateProvince.notIn = fieldFilter.notIn
		}

		if (filter.postalCode) {
			const fieldFilter = filter.postalCode
			where.postalCode = {}
			
			if (fieldFilter.equals !== undefined) where.postalCode.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.postalCode.not = fieldFilter.not
			if (fieldFilter.contains) where.postalCode.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.postalCode.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.postalCode.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.postalCode.in = fieldFilter.in
			if (fieldFilter.notIn) where.postalCode.notIn = fieldFilter.notIn
		}

		if (filter.country) {
			const fieldFilter = filter.country
			where.country = {}
			
			if (fieldFilter.equals !== undefined) where.country.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.country.not = fieldFilter.not
			if (fieldFilter.contains) where.country.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.country.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.country.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.country.in = fieldFilter.in
			if (fieldFilter.notIn) where.country.notIn = fieldFilter.notIn
		}

		if (filter.AND) {
			where.AND = filter.AND.map((f) => this.buildAddressFilter(f))
		}
		if (filter.OR) {
			where.OR = filter.OR.map((f) => this.buildAddressFilter(f))
		}

		return where
	}

	static buildOrderFilter(filter?)) return {}

		const where: any = {}

		
		if (filter.id) {
			const fieldFilter = filter.id
			where.id = {}
			
			if (fieldFilter.equals !== undefined) where.id.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.id.not = fieldFilter.not
			if (fieldFilter.contains) where.id.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.id.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.id.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.id.in = fieldFilter.in
			if (fieldFilter.notIn) where.id.notIn = fieldFilter.notIn
		}

		if (filter.orderDate) {
			const fieldFilter = filter.orderDate
			where.orderDate = {}
			
			if (fieldFilter.equals !== undefined) where.orderDate.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.orderDate.not = fieldFilter.not
			if (fieldFilter.gt !== undefined) where.orderDate.gt = fieldFilter.gt
			if (fieldFilter.gte !== undefined) where.orderDate.gte = fieldFilter.gte
			if (fieldFilter.lt !== undefined) where.orderDate.lt = fieldFilter.lt
			if (fieldFilter.lte !== undefined) where.orderDate.lte = fieldFilter.lte
			if (fieldFilter.in) where.orderDate.in = fieldFilter.in
			if (fieldFilter.notIn) where.orderDate.notIn = fieldFilter.notIn
		}

		if (filter.customerId) {
			const fieldFilter = filter.customerId
			where.customerId = {}
			
			if (fieldFilter.equals !== undefined) where.customerId.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.customerId.not = fieldFilter.not
			if (fieldFilter.contains) where.customerId.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.customerId.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.customerId.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.customerId.in = fieldFilter.in
			if (fieldFilter.notIn) where.customerId.notIn = fieldFilter.notIn
		}

		if (filter.totalAmount) {
			const fieldFilter = filter.totalAmount
			where.totalAmount = {}
			
			if (fieldFilter.equals !== undefined) where.totalAmount.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.totalAmount.not = fieldFilter.not
			if (fieldFilter.gt !== undefined) where.totalAmount.gt = fieldFilter.gt
			if (fieldFilter.gte !== undefined) where.totalAmount.gte = fieldFilter.gte
			if (fieldFilter.lt !== undefined) where.totalAmount.lt = fieldFilter.lt
			if (fieldFilter.lte !== undefined) where.totalAmount.lte = fieldFilter.lte
			if (fieldFilter.in) where.totalAmount.in = fieldFilter.in
			if (fieldFilter.notIn) where.totalAmount.notIn = fieldFilter.notIn
		}

		if (filter.paymentMethod) {
			const fieldFilter = filter.paymentMethod
			where.paymentMethod = {}
			
			if (fieldFilter.equals !== undefined) where.paymentMethod.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.paymentMethod.not = fieldFilter.not
			if (fieldFilter.contains) where.paymentMethod.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.paymentMethod.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.paymentMethod.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.paymentMethod.in = fieldFilter.in
			if (fieldFilter.notIn) where.paymentMethod.notIn = fieldFilter.notIn
		}

		if (filter.specialInstructions) {
			const fieldFilter = filter.specialInstructions
			where.specialInstructions = {}
			
			if (fieldFilter.equals !== undefined) where.specialInstructions.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.specialInstructions.not = fieldFilter.not
			if (fieldFilter.contains) where.specialInstructions.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.specialInstructions.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.specialInstructions.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.specialInstructions.in = fieldFilter.in
			if (fieldFilter.notIn) where.specialInstructions.notIn = fieldFilter.notIn
		}

		if (filter.AND) {
			where.AND = filter.AND.map((f) => this.buildOrderFilter(f))
		}
		if (filter.OR) {
			where.OR = filter.OR.map((f) => this.buildOrderFilter(f))
		}

		return where
	}

	static buildOrderItemFilter(filter?)) return {}

		const where: any = {}

		
		if (filter.id) {
			const fieldFilter = filter.id
			where.id = {}
			
			if (fieldFilter.equals !== undefined) where.id.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.id.not = fieldFilter.not
			if (fieldFilter.contains) where.id.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.id.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.id.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.id.in = fieldFilter.in
			if (fieldFilter.notIn) where.id.notIn = fieldFilter.notIn
		}

		if (filter.orderId) {
			const fieldFilter = filter.orderId
			where.orderId = {}
			
			if (fieldFilter.equals !== undefined) where.orderId.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.orderId.not = fieldFilter.not
			if (fieldFilter.contains) where.orderId.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.orderId.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.orderId.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.orderId.in = fieldFilter.in
			if (fieldFilter.notIn) where.orderId.notIn = fieldFilter.notIn
		}

		if (filter.productId) {
			const fieldFilter = filter.productId
			where.productId = {}
			
			if (fieldFilter.equals !== undefined) where.productId.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.productId.not = fieldFilter.not
			if (fieldFilter.contains) where.productId.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.productId.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.productId.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.productId.in = fieldFilter.in
			if (fieldFilter.notIn) where.productId.notIn = fieldFilter.notIn
		}

		if (filter.quantity) {
			const fieldFilter = filter.quantity
			where.quantity = {}
			
			if (fieldFilter.equals !== undefined) where.quantity.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.quantity.not = fieldFilter.not
			if (fieldFilter.gt !== undefined) where.quantity.gt = fieldFilter.gt
			if (fieldFilter.gte !== undefined) where.quantity.gte = fieldFilter.gte
			if (fieldFilter.lt !== undefined) where.quantity.lt = fieldFilter.lt
			if (fieldFilter.lte !== undefined) where.quantity.lte = fieldFilter.lte
			if (fieldFilter.in) where.quantity.in = fieldFilter.in
			if (fieldFilter.notIn) where.quantity.notIn = fieldFilter.notIn
		}

		if (filter.unitPrice) {
			const fieldFilter = filter.unitPrice
			where.unitPrice = {}
			
			if (fieldFilter.equals !== undefined) where.unitPrice.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.unitPrice.not = fieldFilter.not
			if (fieldFilter.gt !== undefined) where.unitPrice.gt = fieldFilter.gt
			if (fieldFilter.gte !== undefined) where.unitPrice.gte = fieldFilter.gte
			if (fieldFilter.lt !== undefined) where.unitPrice.lt = fieldFilter.lt
			if (fieldFilter.lte !== undefined) where.unitPrice.lte = fieldFilter.lte
			if (fieldFilter.in) where.unitPrice.in = fieldFilter.in
			if (fieldFilter.notIn) where.unitPrice.notIn = fieldFilter.notIn
		}

		if (filter.itemDiscount) {
			const fieldFilter = filter.itemDiscount
			where.itemDiscount = {}
			
			if (fieldFilter.equals !== undefined) where.itemDiscount.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.itemDiscount.not = fieldFilter.not
			if (fieldFilter.gt !== undefined) where.itemDiscount.gt = fieldFilter.gt
			if (fieldFilter.gte !== undefined) where.itemDiscount.gte = fieldFilter.gte
			if (fieldFilter.lt !== undefined) where.itemDiscount.lt = fieldFilter.lt
			if (fieldFilter.lte !== undefined) where.itemDiscount.lte = fieldFilter.lte
			if (fieldFilter.in) where.itemDiscount.in = fieldFilter.in
			if (fieldFilter.notIn) where.itemDiscount.notIn = fieldFilter.notIn
		}

		if (filter.AND) {
			where.AND = filter.AND.map((f) => this.buildOrderItemFilter(f))
		}
		if (filter.OR) {
			where.OR = filter.OR.map((f) => this.buildOrderItemFilter(f))
		}

		return where
	}

	static buildCategoryFilter(filter?)) return {}

		const where: any = {}

		
		if (filter.id) {
			const fieldFilter = filter.id
			where.id = {}
			
			if (fieldFilter.equals !== undefined) where.id.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.id.not = fieldFilter.not
			if (fieldFilter.contains) where.id.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.id.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.id.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.id.in = fieldFilter.in
			if (fieldFilter.notIn) where.id.notIn = fieldFilter.notIn
		}

		if (filter.name) {
			const fieldFilter = filter.name
			where.name = {}
			
			if (fieldFilter.equals !== undefined) where.name.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.name.not = fieldFilter.not
			if (fieldFilter.contains) where.name.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.name.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.name.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.name.in = fieldFilter.in
			if (fieldFilter.notIn) where.name.notIn = fieldFilter.notIn
		}

		if (filter.description) {
			const fieldFilter = filter.description
			where.description = {}
			
			if (fieldFilter.equals !== undefined) where.description.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.description.not = fieldFilter.not
			if (fieldFilter.contains) where.description.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.description.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.description.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.description.in = fieldFilter.in
			if (fieldFilter.notIn) where.description.notIn = fieldFilter.notIn
		}

		if (filter.parentId) {
			const fieldFilter = filter.parentId
			where.parentId = {}
			
			if (fieldFilter.equals !== undefined) where.parentId.equals = fieldFilter.equals
			if (fieldFilter.not !== undefined) where.parentId.not = fieldFilter.not
			if (fieldFilter.contains) where.parentId.contains = fieldFilter.contains
			if (fieldFilter.startsWith) where.parentId.startsWith = fieldFilter.startsWith
			if (fieldFilter.endsWith) where.parentId.endsWith = fieldFilter.endsWith
			if (fieldFilter.in) where.parentId.in = fieldFilter.in
			if (fieldFilter.notIn) where.parentId.notIn = fieldFilter.notIn
		}

		if (filter.AND) {
			where.AND = filter.AND.map((f) => this.buildCategoryFilter(f))
		}
		if (filter.OR) {
			where.OR = filter.OR.map((f) => this.buildCategoryFilter(f))
		}

		return where
	}
}

export class SortBuilder {
	static buildGeneric(sort, allowedFields)) return {}

		const orderBy: any = {}

		for (const field of allowedFields) {
			if (sort[field]) {
				orderBy[field] = sort[field].toLowerCase()
			}
		}

		return orderBy
	}

	
	static buildCustomerSort(sort?)) return { id}

		const orderBy: any = {}

		
		if (sort.id) {
			orderBy.id = sort.id.toLowerCase()
		}

		if (sort.firstName) {
			orderBy.firstName = sort.firstName.toLowerCase()
		}

		if (sort.lastName) {
			orderBy.lastName = sort.lastName.toLowerCase()
		}

		if (sort.emailAddress) {
			orderBy.emailAddress = sort.emailAddress.toLowerCase()
		}

		if (sort.registrationDate) {
			orderBy.registrationDate = sort.registrationDate.toLowerCase()
		}

		if (sort.loyaltyPoints) {
			orderBy.loyaltyPoints = sort.loyaltyPoints.toLowerCase()
		}

		if (sort.shippingAddressId) {
			orderBy.shippingAddressId = sort.shippingAddressId.toLowerCase()
		}

		if (sort.billingAddressId) {
			orderBy.billingAddressId = sort.billingAddressId.toLowerCase()
		}

		if (sort.phoneNumber) {
			orderBy.phoneNumber = sort.phoneNumber.toLowerCase()
		}

		return Object.keys(orderBy).length > 0 ? orderBy )) return { id}

		const orderBy: any = {}

		
		if (sort.id) {
			orderBy.id = sort.id.toLowerCase()
		}

		if (sort.name) {
			orderBy.name = sort.name.toLowerCase()
		}

		if (sort.description) {
			orderBy.description = sort.description.toLowerCase()
		}

		if (sort.price) {
			orderBy.price = sort.price.toLowerCase()
		}

		if (sort.stockQuantity) {
			orderBy.stockQuantity = sort.stockQuantity.toLowerCase()
		}

		return Object.keys(orderBy).length > 0 ? orderBy )) return { id}

		const orderBy: any = {}

		
		if (sort.id) {
			orderBy.id = sort.id.toLowerCase()
		}

		if (sort.streetLine1) {
			orderBy.streetLine1 = sort.streetLine1.toLowerCase()
		}

		if (sort.streetLine2) {
			orderBy.streetLine2 = sort.streetLine2.toLowerCase()
		}

		if (sort.city) {
			orderBy.city = sort.city.toLowerCase()
		}

		if (sort.stateProvince) {
			orderBy.stateProvince = sort.stateProvince.toLowerCase()
		}

		if (sort.postalCode) {
			orderBy.postalCode = sort.postalCode.toLowerCase()
		}

		if (sort.country) {
			orderBy.country = sort.country.toLowerCase()
		}

		return Object.keys(orderBy).length > 0 ? orderBy )) return { id}

		const orderBy: any = {}

		
		if (sort.id) {
			orderBy.id = sort.id.toLowerCase()
		}

		if (sort.orderDate) {
			orderBy.orderDate = sort.orderDate.toLowerCase()
		}

		if (sort.customerId) {
			orderBy.customerId = sort.customerId.toLowerCase()
		}

		if (sort.totalAmount) {
			orderBy.totalAmount = sort.totalAmount.toLowerCase()
		}

		if (sort.paymentMethod) {
			orderBy.paymentMethod = sort.paymentMethod.toLowerCase()
		}

		if (sort.specialInstructions) {
			orderBy.specialInstructions = sort.specialInstructions.toLowerCase()
		}

		return Object.keys(orderBy).length > 0 ? orderBy )) return { id}

		const orderBy: any = {}

		
		if (sort.id) {
			orderBy.id = sort.id.toLowerCase()
		}

		if (sort.orderId) {
			orderBy.orderId = sort.orderId.toLowerCase()
		}

		if (sort.productId) {
			orderBy.productId = sort.productId.toLowerCase()
		}

		if (sort.quantity) {
			orderBy.quantity = sort.quantity.toLowerCase()
		}

		if (sort.unitPrice) {
			orderBy.unitPrice = sort.unitPrice.toLowerCase()
		}

		if (sort.itemDiscount) {
			orderBy.itemDiscount = sort.itemDiscount.toLowerCase()
		}

		return Object.keys(orderBy).length > 0 ? orderBy )) return { id}

		const orderBy: any = {}

		
		if (sort.id) {
			orderBy.id = sort.id.toLowerCase()
		}

		if (sort.name) {
			orderBy.name = sort.name.toLowerCase()
		}

		if (sort.description) {
			orderBy.description = sort.description.toLowerCase()
		}

		if (sort.parentId) {
			orderBy.parentId = sort.parentId.toLowerCase()
		}

		return Object.keys(orderBy).length > 0 ? orderBy ){
		const relationFields = ['shippingAddress', 'billingAddress', 'orders']
		return buildPrismaInclude(info, relationFields)
	}

	static buildProductItemInclude(info){
		const relationFields = ['categories', 'orderItems']
		return buildPrismaInclude(info, relationFields)
	}

	static buildAddressInclude(info){
		const relationFields = ['customerShipping', 'customerBilling']
		return buildPrismaInclude(info, relationFields)
	}

	static buildOrderInclude(info){
		const relationFields = ['customer', 'orderItems']
		return buildPrismaInclude(info, relationFields)
	}

	static buildOrderItemInclude(info){
		const relationFields = ['order', 'product']
		return buildPrismaInclude(info, relationFields)
	}

	static buildCategoryInclude(info){
		const relationFields = ['products', 'parentCategory', 'subCategories']
		return buildPrismaInclude(info, relationFields)
	}
}

export const CUSTOMER_INCLUDES = {
	shippingAddress,
	billingAddress,
	orders}

export const PRODUCTITEM_INCLUDES = {
	categories,
	orderItems}

export const ADDRESS_INCLUDES = {
	customerShipping,
	customerBilling}

export const ORDER_INCLUDES = {
	customer,
	orderItems}

export const ORDERITEM_INCLUDES = {
	order,
	product}

export const CATEGORY_INCLUDES = {
	products,
	parentCategory,
	subCategories}