import { PrismaClient } from '@prisma/client'

export interface Context {
	prisma: PrismaClient
	request: Request
}

export const resolvers = {
	Query: {
		user: (_parent: any, args: { id: string }, ctx: Context) => {
			return ctx.prisma.user.findUnique({
				where: { id: args.id },
			})
		},
		users: (_parent: any, args: { first?: number; after?: string }, ctx: Context) => {
			return ctx.prisma.user.findMany({
				take: args.first || 10,
				skip: args.after ? 1 : 0,
			})
		},

		post: (_parent: any, args: { id: string }, ctx: Context) => {
			return ctx.prisma.post.findUnique({
				where: { id: args.id },
			})
		},
		posts: (_parent: any, args: { first?: number; after?: string; published?: boolean }, ctx: Context) => {
			return ctx.prisma.post.findMany({
				where: args.published !== undefined ? { published: args.published } : undefined,
				take: args.first || 10,
				skip: args.after ? 1 : 0,
			})
		},

		category: (_parent: any, args: { id: string }, ctx: Context) => {
			return ctx.prisma.category.findUnique({
				where: { id: args.id },
			})
		},
		categories: (_parent: any, args: { first?: number; after?: string }, ctx: Context) => {
			return ctx.prisma.category.findMany({
				take: args.first || 20,
				skip: args.after ? 1 : 0,
			})
		},
	},

	User: {
		posts: (parent: { id: string }, args: { first?: number }, ctx: Context) => {
			return ctx.prisma.post.findMany({
				where: { authorId: parent.id },
				take: args.first || 10,
			})
		},
		comments: (parent: { id: string }, args: { first?: number }, ctx: Context) => {
			return ctx.prisma.comment.findMany({
				where: { authorId: parent.id },
				take: args.first || 15,
			})
		},
	},

	Post: {
		author: (parent: { authorId: string }, _args: {}, ctx: Context) => {
			return ctx.prisma.user.findUnique({
				where: { id: parent.authorId },
			})
		},
		categories: async (parent: { id: string }, _args: {}, ctx: Context) => {
			const categoryOnPosts = await ctx.prisma.categoryOnPost.findMany({
				where: { postId: parent.id },
				include: { category: true },
			})
			return categoryOnPosts.map((cop) => cop.category)
		},
		comments: (parent: { id: string }, args: { first?: number }, ctx: Context) => {
			return ctx.prisma.comment.findMany({
				where: { postId: parent.id },
				take: args.first || 15,
			})
		},
	},

	Category: {
		posts: async (parent: { id: string }, args: { first?: number }, ctx: Context) => {
			const categoryOnPosts = await ctx.prisma.categoryOnPost.findMany({
				where: { categoryId: parent.id },
				include: { post: true },
				take: args.first || 20,
			})
			return categoryOnPosts.map((cop) => cop.post)
		},
	},

	Comment: {
		author: (parent: { authorId: string }, _args: {}, ctx: Context) => {
			return ctx.prisma.user.findUnique({
				where: { id: parent.authorId },
			})
		},
		post: (parent: { postId: string }, _args: {}, ctx: Context) => {
			return ctx.prisma.post.findUnique({
				where: { id: parent.postId },
			})
		},
	},

	PostCategory: {
		post: (parent: { postId: string }, _args: {}, ctx: Context) => {
			return ctx.prisma.post.findUnique({
				where: { id: parent.postId },
			})
		},
		category: (parent: { categoryId: string }, _args: {}, ctx: Context) => {
			return ctx.prisma.category.findUnique({
				where: { id: parent.categoryId },
			})
		},
	},

	Mutation: {
		createUser: (_parent: any, args: { email: string; name: string; bio?: string }, ctx: Context) => {
			return ctx.prisma.user.create({
				data: {
					email: args.email,
					name: args.name,
					bio: args.bio,
				},
			})
		},

		createPost: (_parent: any, args: { title: string; content: string; published?: boolean; authorId: string }, ctx: Context) => {
			return ctx.prisma.post.create({
				data: {
					title: args.title,
					content: args.content,
					published: args.published ?? false,
					authorId: args.authorId,
				},
			})
		},

		createComment: (_parent: any, args: { content: string; postId: string; authorId: string }, ctx: Context) => {
			return ctx.prisma.comment.create({
				data: {
					content: args.content,
					postId: args.postId,
					authorId: args.authorId,
				},
			})
		},

		createCategory: (_parent: any, args: { name: string; description?: string }, ctx: Context) => {
			return ctx.prisma.category.create({
				data: {
					name: args.name,
					description: args.description,
				},
			})
		},

		addCategoryToPost: (_parent: any, args: { postId: string; categoryId: string }, ctx: Context) => {
			return ctx.prisma.categoryOnPost.create({
				data: {
					postId: args.postId,
					categoryId: args.categoryId,
				},
			})
		},
	},
}
