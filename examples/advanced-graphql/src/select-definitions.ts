import type { Prisma } from '@prisma/client'

export const PRODUCT_BASE_SELECT = {
	id: true,
	createdAt: true,
	updatedAt: true,
	name: true,
	description: true,
	price: true,
	status: true,
	metadata: true,
} satisfies Prisma.ProductSelect

export const REVIEW_BASE_SELECT = {
	id: true,
	createdAt: true,
	title: true,
	content: true,
	rating: true,
	verified: true,
	helpfulCount: true,
	productId: true,
} satisfies Prisma.ReviewSelect

export const TAG_BASE_SELECT = {
	id: true,
	name: true,
	color: true,
} satisfies Prisma.TagSelect

export const PRODUCT_WITH_REVIEWS_SELECT = {
	...PRODUCT_BASE_SELECT,
	reviews: {
		select: REVIEW_BASE_SELECT,
	},
} satisfies Prisma.ProductSelect

export const PRODUCT_WITH_TAGS_SELECT = {
	...PRODUCT_BASE_SELECT,
	tags: {
		select: {
			productId: true,
			tagId: true,
			assignedAt: true,
			product: {
				select: PRODUCT_BASE_SELECT,
			},
			tag: {
				select: TAG_BASE_SELECT,
			},
		},
	},
} satisfies Prisma.ProductSelect

export const PRODUCT_WITH_ALL_RELATIONS_SELECT = {
	...PRODUCT_BASE_SELECT,
	reviews: {
		select: REVIEW_BASE_SELECT,
	},
	tags: {
		select: {
			productId: true,
			tagId: true,
			assignedAt: true,
			product: {
				select: PRODUCT_BASE_SELECT,
			},
			tag: {
				select: TAG_BASE_SELECT,
			},
		},
	},
} satisfies Prisma.ProductSelect

export const REVIEW_WITH_PRODUCT_SELECT = {
	...REVIEW_BASE_SELECT,
	product: {
		select: PRODUCT_BASE_SELECT,
	},
} satisfies Prisma.ReviewSelect

export const TAG_WITH_PRODUCTS_SELECT = {
	...TAG_BASE_SELECT,
	products: {
		select: {
			productId: true,
			tagId: true,
			assignedAt: true,
			product: {
				select: PRODUCT_BASE_SELECT,
			},
			tag: {
				select: TAG_BASE_SELECT,
			},
		},
	},
} satisfies Prisma.TagSelect

export const PRODUCT_TAG_BASE_SELECT = {
	productId: true,
	tagId: true,
	assignedAt: true,
} satisfies Prisma.ProductTagSelect

export const PRODUCT_TAG_WITH_RELATIONS_SELECT = {
	...PRODUCT_TAG_BASE_SELECT,
	product: {
		select: PRODUCT_BASE_SELECT,
	},
	tag: {
		select: TAG_BASE_SELECT,
	},
} satisfies Prisma.ProductTagSelect

export type ProductWithReviews = Prisma.ProductGetPayload<{
	select: typeof PRODUCT_WITH_REVIEWS_SELECT
}>

export type ProductWithTags = Prisma.ProductGetPayload<{
	select: typeof PRODUCT_WITH_TAGS_SELECT
}>

export type ProductWithAllRelations = Prisma.ProductGetPayload<{
	select: typeof PRODUCT_WITH_ALL_RELATIONS_SELECT
}>

export type ReviewWithProduct = Prisma.ReviewGetPayload<{
	select: typeof REVIEW_WITH_PRODUCT_SELECT
}>

export type TagWithProducts = Prisma.TagGetPayload<{
	select: typeof TAG_WITH_PRODUCTS_SELECT
}>

export type ProductTagWithRelations = Prisma.ProductTagGetPayload<{
	select: typeof PRODUCT_TAG_WITH_RELATIONS_SELECT
}>
